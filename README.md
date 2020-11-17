# Proyecto Final Bases de Datos Avanzadas

## Tecnológico de Costa Rica
#### Proyecto - Final
##### Intregrantes:
  - Kevin Hernández
  - Silvia Calderón
  - Oswaldo Dávila
 
Este proyecto contiene la solución de los problemas:

## Problema 01 

Termine la implementación de MapReduce que se le da en erlang. Esta implementación la debe probar ejecutándola en varios nodos distribuidos (distributed erlang) tal que pueda configurar libremente la cantidad de tareas Map, tareas Reduce, tamaño de los chunks, y ubicación de cada una de las tareas en su red de nodos erlang.

La entrada de su mapReduce la debe hacer desde un archivo que contenga términos erlang de la forma `{llave, num1, num2}`, las tareas map deben sumar los números y dejar un solo registro de `{llave, num}`, las tareas reduce, deben sumar todos los números asociados a una misma llave.

## Problema 02

En este segundo ejercicio debe estructurar su código tal que lo que se comporte bajo el behavior de gen server, esté comportamiento de OTP permite generar servidores robustos, con políticas de ejecución y cambio de código en caliente, si es necesario.

## Problema 03

En este apartado de la tarea debe estructurar su código dentro de una aplicación (application) del OTP, las aplicaciones OTP, en el archivo de configuración de la aplicación puede definir si tiene una jerarquía de procesos de supervisión, los cuales le permiten definir políticas en caso de que alguno de los procesos se caiga.

Una vez hecho esto debe garantizar que su MapReduce funcione de manera distribuida, y que siga las políticas de restart o abort, definidas para los procesos map, reduce, y orquestador general que se esbozan en el capítulo 2 del libro MMDS.

### Solución a problemas 01-03

La solución a estos problemas se encuentra en el apartado MapReduce de este documento.

## Problema 04 

Implemente ahora la tarea de multiplicar una matriz por un vector, debe suponer que la matriz es rala y gigantesca, esto es, la matriz cumple con las propiedades de la matriz utilizada por el PageRank. Tanto el vector como la matriz de multiplicación las debe cargar de un archivo.

El formato de la matriz es el simple: cada linea es una página, que nada más contiene una lista pequeña de números de páginas a las cuales hace link. El vector es arbitrario.
Asuma que el tamaño del archivo tanto del vector como de la matriz puede que no le quepa en memoria, y que por lo tanto lo debe leer secuencialmente o en bloques.

### Solución al problema 04

La solución a estos problemas se encuentra en el apartado Pagerank de este documento.

## Uso

### Crear dos nodos
```bash
iex --sname nodoA --cookie secret_token -S mix 
iex --sname nodoB --cookie secret_token -S mix 
```

### Generar tuplas

Por ejemplo 100 líneas con números del 1 a n, en este caso 10:

```bash
iex(1)> Generator.tuples(100, 10, "tuples.data")
```

### MapReduce

```bash
iex(nodoB@host)> {:ok, pid} = GenServer.start_link(Parallel, [])
iex(nodoB@host)> GenServer.cast(pid, {:inicio, MapReduce, [{:nodoA@host, 2}], pid})
```

### Pagerank

```bash
iex(nodoB@host)> {:ok, pid} = GenServer.start_link(ParallelV2, [])
iex(nodoB@host)> GenServer.cast(pid, {:inicio, Pagerank, [{:nodoA@host, 1}], pid})
```
