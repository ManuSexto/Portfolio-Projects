
----- Part I: TIME SERIES

--Obtain the time series of the company's global sales for the period between 2011 and 2014. We must identify where the data is stored and create a query to group the sales values ​​by time period (days, weeks, months, etc.). The tables required are:
--SalesOrderDetail: the details of the transactions of an invoice.
--SalesOrderHeader: the invoices and their relevant information.

SELECT *
FROM Sales.SalesOrderHeader

SELECT *
FROM Sales.SalesOrderDetail

-- 1. First query: We must combine both tables through the SalesOrderID, set the aggregation function (Sum) to SELECT and use GROUP BY to specify the grouping mode. For optimal visualization, it is ideal to sort the records by date.

SELECT YEAR(SOH.OrderDate) AS Año
	  ,MONTH(SOH.OrderDate) AS Mes
	  ,DATEPART(iso_week, SOH.OrderDate) AS Semana
	  ,CONVERT(date, SOH.OrderDate) AS Día
	  ,SUM(SOD.LineTotal) AS TotalVentas
FROM Sales.SalesOrderHeader AS SOH
INNER JOIN Sales.SalesOrderDetail AS SOD
	ON SOH.SalesOrderID = SOD.SalesOrderID
WHERE OrderDate BETWEEN '20110101' AND '20141231'
GROUP BY YEAR(SOH.OrderDate)
		,MONTH(SOH.OrderDate)
		,DATEPART(iso_week, SOH.OrderDate)
	    ,CONVERT(date, SOH.OrderDate)
ORDER BY Año
		,Mes
		,Semana
		,Día


--2.	Additionally, we are asked to obtain sales by region (USA, Europe and Pacific), including a final dataset containing the aforementioned variables. There are 3 queries in total.

CREATE VIEW NORTHAMERICA AS 
SELECT [Group] AS REGIÓN
	  ,YEAR(SOH.OrderDate) AS Año
	  ,MONTH(SOH.OrderDate) AS Mes
	  ,DATEPART(iso_week, SOH.OrderDate) AS Semana
	  ,CONVERT(date, SOH.OrderDate) AS Día
	  ,SUM(SOD.LineTotal) AS TotalVentas
FROM Sales.SalesOrderHeader AS SOH
INNER JOIN Sales.SalesOrderDetail AS SOD
	ON SOH.SalesOrderID = SOD.SalesOrderID
LEFT JOIN SALES.SalesTerritory AS ST
	ON ST.TerritoryID = SOH.TerritoryID
	WHERE ST.[GROUP] = 'NORTH AMERICA'
GROUP BY ST.[Group] 
		,YEAR(SOH.OrderDate)
		,MONTH(SOH.OrderDate)
		,DATEPART(iso_week, SOH.OrderDate)
	    ,CONVERT(date, SOH.OrderDate) 
ORDER BY Año
		,Mes
		,Semana
		,Día

CREATE VIEW EUROPE AS 
SELECT [Group] AS REGIÓN
	  ,YEAR(SOH.OrderDate) AS Año
	  ,MONTH(SOH.OrderDate) AS Mes
	  ,DATEPART(iso_week, SOH.OrderDate) AS Semana
	  ,CONVERT(date, SOH.OrderDate) AS Día
	  ,SUM(SOD.LineTotal) AS TotalVentas
FROM Sales.SalesOrderHeader AS SOH
INNER JOIN Sales.SalesOrderDetail AS SOD
	ON SOH.SalesOrderID = SOD.SalesOrderID
LEFT JOIN SALES.SalesTerritory AS ST
	ON ST.TerritoryID = SOH.TerritoryID
	WHERE ST.[GROUP] = 'EUROPE'
GROUP BY ST.[Group] 
		,YEAR(SOH.OrderDate)
		,MONTH(SOH.OrderDate)
		,DATEPART(iso_week, SOH.OrderDate)
	    ,CONVERT(date, SOH.OrderDate) 
ORDER BY Año
		,Mes
		,Semana
		,Día

CREATE VIEW PACIFIC AS
SELECT [Group] AS REGIÓN
	  ,YEAR(SOH.OrderDate) AS Año
	  ,MONTH(SOH.OrderDate) AS Mes
	  ,DATEPART(iso_week, SOH.OrderDate) AS Semana
	  ,CONVERT(date, SOH.OrderDate) AS Día
	  ,SUM(SOD.LineTotal) AS TotalVentas
FROM Sales.SalesOrderHeader AS SOH
INNER JOIN Sales.SalesOrderDetail AS SOD
	ON SOH.SalesOrderID = SOD.SalesOrderID
LEFT JOIN SALES.SalesTerritory AS ST
	ON ST.TerritoryID = SOH.TerritoryID
	WHERE ST.[GROUP] = 'PACIFIC'
GROUP BY ST.[Group] 
		,YEAR(SOH.OrderDate)
		,MONTH(SOH.OrderDate)
		,DATEPART(iso_week, SOH.OrderDate)
	    ,CONVERT(date, SOH.OrderDate) 
ORDER BY Año
		,Mes
		,Semana
		,Día


--3. In order to combine the previous queries we must use the first one as a base query and the other 3 as subqueries. To do this we must identify the key that allows us to combine them . We must choose appropriately between INNER JOIN and LEFT JOIN taking into account that there may be days with sales in certain regions and not in others. 

-- we have created views of the previous queries to incorporate them into the following query

SELECT YEAR(SOH.OrderDate) AS Año
	  ,MONTH(SOH.OrderDate) AS Mes
	  ,DATEPART(iso_week, SOH.OrderDate) AS Semana
	  ,CONVERT(date, SOH.OrderDate) AS Día
	  ,SUM(SOD.LineTotal) AS TotalVentas
	  ,NA.TotalVentas AS NorthAmerica
	  ,EU.TotalVentas AS Europe
	  ,PAC.TotalVentas AS Pacific
FROM Sales.SalesOrderHeader AS SOH
INNER JOIN Sales.SalesOrderDetail AS SOD
	ON SOH.SalesOrderID = SOD.SalesOrderID
LEFT JOIN NORTHAMERICA AS NA
	ON NA.Día = SOH.OrderDate
LEFT JOIN EUROPE AS EU
	ON EU.Día = SOH.OrderDate
LEFT JOIN PACIFIC AS PAC
	ON PAC.Día = SOH.OrderDate
WHERE OrderDate BETWEEN '20110101' AND '20141231'
GROUP BY YEAR(SOH.OrderDate)
		,MONTH(SOH.OrderDate)
		,DATEPART(iso_week, SOH.OrderDate)
	    ,CONVERT(date, SOH.OrderDate)
		,Na.TotalVentas
		,EU.TotalVentas
		,PAC.TotalVentas
ORDER BY Año
		,Mes
		,Semana
		,Día
	


------ PART II: CUSTOMER DATASET FOR LINEAR REGRESSION

--- Obtain the accumulated spending of all customers (individuals, not stores) and their different demographic variables (age, country, income, education, etc.)

SELECT *
FROM Sales.SalesOrderHeader

SELECT *
FROM Person.Person

SELECT *
FROM Sales.SalesTerritory


CREATE VIEW DEMOGRAPHICS
AS 
SELECT 
    p.[BusinessEntityID] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        TotalPurchaseYTD[1]', 'money') AS [TotalPurchaseYTD] 
    ,CONVERT(datetime, REPLACE([IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        DateFirstPurchase[1]', 'nvarchar(20)') ,'Z', ''), 101) AS [DateFirstPurchase] 
    ,CONVERT(datetime, REPLACE([IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        BirthDate[1]', 'nvarchar(20)') ,'Z', ''), 101) AS [BirthDate] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        MaritalStatus[1]', 'nvarchar(1)') AS [MaritalStatus] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        YearlyIncome[1]', 'nvarchar(30)') AS [YearlyIncome] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        Gender[1]', 'nvarchar(1)') AS [Gender] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        TotalChildren[1]', 'integer') AS [TotalChildren] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        NumberChildrenAtHome[1]', 'integer') AS [NumberChildrenAtHome] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        Education[1]', 'nvarchar(30)') AS [Education] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        Occupation[1]', 'nvarchar(30)') AS [Occupation] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        HomeOwnerFlag[1]', 'bit') AS [HomeOwnerFlag] 
    ,[IndividualSurvey].[ref].[value](N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
        NumberCarsOwned[1]', 'integer') AS [NumberCarsOwned] 
FROM [Person].[Person] p 
CROSS APPLY p.[Demographics].nodes(N'declare default element namespace "http://schemas.microsoft.com/sqlserver/2004/07/adventure-works/IndividualSurvey"; 
    /IndividualSurvey') AS [IndividualSurvey](ref) 
WHERE [Demographics] IS NOT NULL

SELECT *
FROM DEMOGRAPHICS


SELECT SUM (SOH.TotalDue) AS GastoTotal
	  ,DATEDIFF(YEAR, DEM.BirthDate, GETDATE()) AS Edad
	  ,ST.Name AS País
	  ,CASE
	  WHEN DEM.Gender = 'M' THEN 'Masculino'
	  WHEN DEM.Gender = 'F' THEN 'Femenino'
	  ELSE	DEM.Gender
	END AS Género
	  ,CASE 
        WHEN DEM.MaritalStatus = 'S' THEN 'Soltero'
        WHEN DEM.MaritalStatus = 'M' THEN 'Casado'
        ELSE DEM.MaritalStatus
    END AS EstadoCivil
	  ,DEM.Education AS Educacion
	  ,DEM.Occupation AS Trabajo
	  ,DEM.YearlyIncome AS IngresosAnuales
	  ,DEM.TotalChildren AS Hijos
	  ,DEM.NumberChildrenAtHome AS HijosEnCasa
	  ,DEM.NumberCarsOwned AS CochesEnPropiedad
FROM Sales.SalesOrderHeader AS SOH
INNER JOIN Person.Person AS PP
	ON PP.BusinessEntityID = SOH.CustomerID
INNER JOIN SALES.SalesTerritory AS ST
	ON ST.TerritoryID = SOH.TerritoryID
INNER JOIN DEMOGRAPHICS AS DEM
	ON DEM.BusinessEntityID = SOH.CustomerID
WHERE PP.PersonType = 'IN'
GROUP BY DATEDIFF(YEAR, DEM.BirthDate, GETDATE())
		,ST.Name
		,DEM.Gender
		,DEM.MaritalStatus
		,DEM.Education
		,DEM.Occupation
		,DEM.TotalChildren
		,DEM.NumberChildrenAtHome
		,DEM.NumberCarsOwned
		,DEM.YearlyIncome
ORDER BY GastoTotal DESC

-- For this query we had to create a view with the information from Demographics broken down, 
-- then we joined the tables and finally created the query of the total spending of individual clients ordered from highest to lowest, with the data offered by Demographics, such as age, country, gender...

-----PART III: CUSTOMER DATASET FOR CLASSIFICATION (LOGISTICAL REGRESSION)


SELECT * FROM Production.ProductSubcategory


 SELECT 
 CASE 
        WHEN PS.ProductCategoryID = '1' THEN '1'
        ELSE '0'
    END AS CompróBici
 FROM Sales.SalesOrderHeader AS SOH
 INNER JOIN Sales.SalesOrderDetail AS SOD
	ON SOD.SalesOrderID = SOH.SalesOrderID
INNER JOIN Sales.Customer AS CUS
	ON CUS.CustomerID = SOH.CustomerID
INNER JOIN Production.Product AS PP
	ON PP.ProductID = SOD.ProductID
INNER JOIN Production.ProductSubcategory AS PS
	ON PS.ProductSubcategoryID = PP.ProductSubcategoryID


SELECT SUM (SOH.TotalDue) AS GastoTotal
	  ,DATEDIFF(YEAR, DEM.BirthDate, GETDATE()) AS Edad
	  ,ST.Name AS País
	  ,CASE
	  WHEN DEM.Gender = 'M' THEN 'Masculino'
	  WHEN DEM.Gender = 'F' THEN 'Femenino'
	  ELSE	DEM.Gender
	END AS Género
	  ,CASE 
        WHEN DEM.MaritalStatus = 'S' THEN 'Soltero'
        WHEN DEM.MaritalStatus = 'M' THEN 'Casado'
        ELSE DEM.MaritalStatus
    END AS EstadoCivil
	  ,DEM.Education AS Educacion
	  ,DEM.Occupation AS Trabajo
	  ,DEM.YearlyIncome AS IngresosAnuales
	  ,DEM.TotalChildren AS Hijos
	  ,DEM.NumberChildrenAtHome AS HijosEnCasa
	  ,DEM.NumberCarsOwned AS CochesEnPropiedad
	  ,BikePurchase.CompróBici
FROM Sales.SalesOrderHeader AS SOH
INNER JOIN Person.Person AS PP
	ON PP.BusinessEntityID = SOH.CustomerID
INNER JOIN SALES.SalesTerritory AS ST
	ON ST.TerritoryID = SOH.TerritoryID
INNER JOIN DEMOGRAPHICS AS DEM
	ON DEM.BusinessEntityID = SOH.CustomerID
INNER JOIN (
				 SELECT SOH.CustomerID
				 ,CASE 
						WHEN PS.ProductCategoryID = '1' THEN '1'
						ELSE '0'
					END AS CompróBici
				 FROM Sales.SalesOrderHeader AS SOH
				 INNER JOIN Sales.SalesOrderDetail AS SOD
					ON SOD.SalesOrderID = SOH.SalesOrderID
				INNER JOIN Sales.Customer AS CUS
					ON CUS.CustomerID = SOH.CustomerID
				INNER JOIN Production.Product AS PP
					ON PP.ProductID = SOD.ProductID
				INNER JOIN Production.ProductSubcategory AS PS
					ON PS.ProductSubcategoryID = PP.ProductSubcategoryID)
					AS BikePurchase
								ON BikePurchase.CustomerID =SOH.CustomerID 
WHERE PP.PersonType = 'IN'
GROUP BY DATEDIFF(YEAR, DEM.BirthDate, GETDATE())
		,ST.Name
		,DEM.Gender
		,DEM.MaritalStatus
		,DEM.Education
		,DEM.Occupation
		,DEM.TotalChildren
		,DEM.NumberChildrenAtHome
		,DEM.NumberCarsOwned
		,DEM.YearlyIncome
		,BikePurchase.CompróBici
ORDER BY GastoTotal DESC


-- First we create a query to find out which customers bought bikes and which ones didn't, coding those who bought as 1 and those who didn't as 0, then we add it as a subquery to the previous query of total spending per customer.

--  Bonus 1:
--Obtain the 10 months in which the most orders (number of orders, quantity of items and total monetary amount) have been placed with suppliers,
--grouped by year and month, taking into account the “Components” category, ordered by total monetary amount.


-- We start with Select top 10 to get the 10 that the statement asks for, we select the columns with date, number of orders, quantity and monetary total. 
--We group tables to obtain the information PurchaseOrderHeader (POH), PurchaseOrderDetail (POD), Product (P), ProductSubcategory (PS) and ProductCategory (PC). We filter by components with where, we group with group by and we sort

CREATE VIEW Top10pedido AS
SELECT TOP 10
    YEAR(OrderDate) AS YEAR,
    MONTH(OrderDate) AS MONTH,
    COUNT(*) AS TOTALORDERS,
    SUM(TotalDue) AS TOTALAMOUNT,
    SUM(OrderQty) AS TOTALQTY
FROM
    Purchasing.PurchaseOrderHeader AS POH
  INNER JOIN Purchasing.PurchaseOrderDetail AS POD ON POH.PurchaseOrderID = POD.PurchaseOrderID
  INNER JOIN Production.Product AS P ON POD.ProductID = P.ProductID
  INNER JOIN Production.ProductSubcategory AS PS ON P.ProductSubcategoryID = PS.ProductSubcategoryID
  INNER JOIN Production.ProductCategory AS PC ON PS.ProductCategoryID = PC.ProductCategoryID
WHERE
    PC.Name = 'Components'
GROUP BY
    YEAR(OrderDate),
    MONTH(OrderDate)
ORDER BY
    TOTALAMOUNT DESC

--BONUS 2:
-- Create a view that returns the top 10 sellers (first and last name) with the most orders, total amount, number of units sold and the total ratio of bicycles to total orders, taking into account only the “Bikes” category.


-- We start again with Select top 10 to assign the 10 that the statement asks us for, we select columns with the name and surname of the seller, total orders with COUNT, sums of total monetary and quantity of bikes sold. 
--We join the salesordearheader tables with salesorderdeatail to obtain the details, we join production.product for the product information, we join the category tables to be able to filter the bike category with where, we group and sort in the TotalAmount function to obtain the Top 10 sellers. 
--At first we thought it was by more orders and we did it in the TOTALSALESORDERS function in descending order to obtain the top with more orders, but it did not resemble the photo of the results to obtain so we did it in the TotalAmount function. 
-- Also, as you can see in the code, I have not been able to calculate the ratio by a little so we have left it unanswered.

CREATE VIEW TOP10BICIVENDEDORES AS

SELECT TOP 10
    P.FirstName AS FIRSTNAME,
    P.LastName AS LASTNAME,
    COUNT(*) AS TOTALSALESORDERS,
	SUM(sod.LineTotal) AS TotalAmount,
    SUM(sod.OrderQty) AS TotalQty
   -- AS RATIO
FROM
    Sales.SalesOrderHeader AS SOH
    INNER JOIN Sales.SalesOrderDetail AS SOD ON SOH.SalesOrderID = SOD.SalesOrderID
    INNER JOIN Sales.SalesPerson AS SP ON SOH.SalesPersonID = SP.BusinessEntityID
    INNER JOIN Person.Person AS P ON SP.BusinessEntityID = P.BusinessEntityID
    INNER JOIN Production.Product AS PR ON SOD.ProductID = PR.ProductID
    INNER JOIN Production.ProductSubcategory AS PSC ON PR.ProductSubcategoryID = PSC.ProductSubcategoryID
    INNER JOIN Production.ProductCategory AS PC ON PSC.ProductCategoryID = PC.ProductCategoryID
WHERE
    PC.Name = 'Bikes'
GROUP BY
    P.FirstName,
    P.LastName
ORDER BY
    TotalAmount DESC

