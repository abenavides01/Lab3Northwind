
-- sql_setup.sql
-- Crea la tabla de usuarios para login/registro en la misma BD de Northwind (SQL Server)
IF NOT EXISTS (SELECT * FROM sys.tables WHERE name = 'AppUsers')
BEGIN
  CREATE TABLE dbo.AppUsers (
    Id INT IDENTITY(1,1) PRIMARY KEY,
    Email NVARCHAR(255) NOT NULL UNIQUE,
    PasswordHash NVARCHAR(255) NOT NULL,
    CreatedAt DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME()
  );
END


USE Northwind

SELECT @@SERVERNAME AS servername, @@SERVICENAME AS instancename;
SELECT net_transport, local_tcp_port
FROM sys.dm_exec_connections
WHERE session_id = @@SPID;


EXEC xp_readerrorlog 0, 1, N'Server is listening on';
SELECT net_transport, local_tcp_port
FROM sys.dm_exec_connections
WHERE session_id = @@SPID;



SELECT name FROM sys.objects WHERE type = 'U' ORDER BY name;

IF OBJECT_ID(N'[dbo].[Order Details]', N'U') IS NULL
AND OBJECT_ID(N'[dbo].[OrderDetails]', N'U') IS NOT NULL
BEGIN
    CREATE SYNONYM [dbo].[Order Details] FOR [dbo].[OrderDetails];
END;

SELECT TOP 1 * FROM [dbo].[Order Details];
SELECT TOP 1 * FROM dbo.Invoices;

SELECT * FROM Customers