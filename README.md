
ELEVADOR (PROYECTO)
=====
Por: Grupo Nro 3
-----

## Installation

1. Se debe contar con la Instalación de Rebar3 y Erlang en nuestra PC
2. Para ejecutar el proyecto se debe ingresar los siguientes comandos:

    
## Ejecución

To deploy this project run

```bash
  rebar3 get-deps
  rebar3 compile
  rebar3 shell
```
Inicializamos nuestros archivos erlang
```bash
  elevator_http:start().
  elevator:start().
```
Ingresamos a nuestro puerto 8080
```bash
  http://localhost:8080/
```
Y Listo Visualizamos nuestro proyecto en Ejecución 🧨