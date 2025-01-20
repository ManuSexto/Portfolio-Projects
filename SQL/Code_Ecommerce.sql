

-- 1. Provide a list of products consisting of name, product number and color for those 
-- with a price greater than 20 euros and sizes XS-XL.

SELECT Nombre, NumeroProducto, Color
FROM EC_Productos 
WHERE Talla_disponibles LIKE ('XS-XL') AND Precio > 20;


-- 2. Provide a list of individual clients composed of ClientID, name,
-- surname and gender who were born between 1970 and 1980, whose occupation is not
-- researcher, ordered by date of first purchase in descending order

SELECT IDCliente, Nombre, Apellidos, Genero
FROM EC_Clientes_IN
WHERE YEAR(Fecha_Nacimiento) BETWEEN 1970 AND 1980 AND Ocupacion NOT LIKE 'Investigador'
ORDER BY Fecha_Primera_Compra DESC;


-- 3. Obtain a list consisting of invoice, order date, shipping date and
-- order status containing codes 9658 and 4568 in the observations.


SELECT IDFactura, FechaPedido, FechaEnvio, EstadoPedido, Observaciones
FROM EC_Facturas
WHERE Observaciones LIKE '%9658%' OR Observaciones LIKE '%4568%';


-- 4. Provide a list of InvoiceID, CustomerID, order date and total with taxes whose status is canceled and the total with taxes is greater than 1000.

SELECT IDFactura, IDCliente, FechaPedido, Total_con_Impuestos
FROM EC_Facturas
WHERE Total_con_Impuestos > 1000 AND EstadoPedido LIKE 'Cancelado';


-- 5. Using the previous query as a base, and using it as a subquery, 
-- obtain the company name and telephone number of those clients.

SELECT DenominacionSocial, Telefono, IDCliente
FROM EC_Clientes_EM
WHERE IDCliente IN (
    SELECT CL.IDCliente
    FROM EC_Facturas as F
    RIGHT JOIN EC_Clientes as CL
        ON F.IDCliente = CL.IDCliente
    LEFT JOIN EC_Clientes_IN CL_IN
        ON CL_IN.IDCliente = CL.IDCliente
    LEFT JOIN EC_Clientes_EM CL_EM
        ON CL_EM.IDCliente = CL.IDCliente
    WHERE F.Total_con_Impuestos > 1000 
        AND F.EstadoPedido = 'Cancelado'
        AND (CL_IN.Telefono IS NOT NULL
             OR CL_EM.DenominacionSocial IS NOT NULL
             OR CL_EM.Telefono IS NOT NULL)
);

-- 6. Obtain a list consisting of invoice, product name, color, price 
-- unit, quantity and % discount for transactions made between April and September 2019


SELECT TR.IDFactura, PR.Nombre, PR.Color, PR.Precio, TR.Cantidad, TR.Descuento, FC.FechaPedido
FROM EC_Transacciones AS TR
inner JOIN EC_Facturas AS FC
	 ON TR.IDFactura = FC.IDFactura
LEFT JOIN EC_Productos AS PR
	ON PR.IDProducto = TR.IDProducto
WHERE FC.FechaPedido BETWEEN '2019-04-01' AND '2019-09-30';



-- 7. You want to know how many products there are in each product category, as well as
-- the maximum price, minimum price and average price for each category, ordered from highest to lowest in
-- based on the count by category.

SELECT GrupoProductoID, 
COUNT(IDProducto) AS TotalProductos, 
MAX(Precio) AS PrecioMaximo, 
MIN(Precio) AS PrecioMinimo, 
AVG(Precio) AS PRECIO_MEDIO
FROM EC_Productos
GROUP BY GrupoProductoID
ORDER BY TotalProductos DESC;



-- 8. Get total sales with taxes by country and region. Excluding cancelled orders. Sorted from lowest to highest by total sales


SELECT T.Pais, T.Region, SUM(F.Total_con_Impuestos) AS Ventas_Totales
FROM EC_Facturas AS F
INNER JOIN EC_Clientes AS C
	ON C.IDCliente = F.IDCliente
INNER JOIN EC_Territorio as T
	ON C.TerritorioID = T.TerritorioID
WHERE F.EstadoPedido NOT LIKE 'Cancelado'
GROUP BY T.Pais, T.Region
ORDER BY Ventas_Totales ASC;


-- 9. We want to know the number of orders, the total amount without taxes for
-- individual customers, as well as their name and account number. We only
-- want those whose total amount exceeds 1500 euros. Sort the
-- result from highest to lowest based on the total amount calculated.

SELECT CL_IN.Nombre, CL.NumeroCuenta, COUNT(F.IDFactura) AS PEDIDOS, SUM(F.Total) AS TotalSinImpuestos
FROM EC_Facturas AS F
INNER JOIN EC_Clientes AS CL
	ON CL.IDCliente = F.IDCliente
RIGHT JOIN EC_Clientes_IN AS CL_IN
	ON CL.IDCliente = CL_IN.IDCliente
GROUP BY CL_IN.Nombre, CL.NumeroCuenta
HAVING SUM(F.Total) > 1500
ORDER BY SUM(F.Total) DESC;


-- Get a list of the best-selling products. The query must include the product name, 
-- the category name, the total quantity sold, and the total sales without taxes. 
-- The results must be ordered from highest to lowest by total quantity sold.

SELECT PROD.Nombre
	  ,CAT.Nombre
	  ,SUM(TRA.Cantidad) AS Total_unidades_vendidas
	  ,SUM(FACT.Total) AS Total_venta_sin_impuestos
FROM EC_Productos AS PROD
INNER JOIN EC_Cat_Productos AS CAT
	ON PROD.GrupoProductoID = CAT.GrupoProductoID
INNER JOIN EC_Transacciones AS TRA
	ON TRA.IDProducto = PROD.IDProducto
INNER JOIN EC_Facturas AS FACT
	ON FACT.IDFactura = TRA.IDFactura
GROUP BY PROD.Nombre
	  ,CAT.Nombre
ORDER BY Total_unidades_vendidas DESC;


-- Obtain a list of products that have had sales above the average sales of all products. 
-- The query must include the product name, the category name, 
-- and the total units sold above the average.

SELECT 
	  PROD.Nombre AS NombreProducto
	  ,CAT.Nombre AS NombreCategoria
	  ,SUM(TRA.Cantidad)AS CantidadTotal
FROM EC_Productos AS PROD
INNER JOIN EC_Cat_Productos AS CAT
	ON PROD.GrupoProductoID = CAT.GrupoProductoID
INNER JOIN EC_Transacciones AS TRA
	ON TRA.IDProducto = PROD.IDProducto
GROUP BY PROD.Nombre
	    ,CAT.Nombre 
HAVING 
    SUM(TRA.Cantidad) > (SELECT AVG(TotalVentas)
        FROM 
            (SELECT 
                SUM(TRA.Cantidad) AS TotalVentas
            FROM 
                EC_Productos AS PROD
            INNER JOIN 
                EC_Transacciones AS TRA
                ON TRA.IDProducto = PROD.IDProducto
			GROUP BY PROD.IDProducto) 
		AS SubconsultaPromedio)
ORDER BY CantidadTotal DESC;











