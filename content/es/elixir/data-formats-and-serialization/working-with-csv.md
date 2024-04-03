---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:24.563844-07:00
description: "Trabajar con archivos CSV (Valores Separados por Comas) implica leer\
  \ y escribir datos en estos archivos, una necesidad com\xFAn para tareas que requieren\
  \ la\u2026"
lastmod: '2024-03-13T22:44:58.725827-06:00'
model: gpt-4-0125-preview
summary: "Trabajar con archivos CSV (Valores Separados por Comas) implica leer y escribir\
  \ datos en estos archivos, una necesidad com\xFAn para tareas que requieren la importaci\xF3\
  n/exportaci\xF3n de datos o soluciones de almacenamiento simples."
title: Trabajando con CSV
weight: 37
---

## Cómo hacerlo:
Elixir, con su poderoso emparejamiento de patrones y soporte para la canalización, puede manejar archivos CSV de manera eficiente, incluso sin bibliotecas de terceros. Sin embargo, para necesidades más avanzadas, la biblioteca `nimble_csv` es una opción rápida y sencilla.

### Leyendo un Archivo CSV Sin Bibliotecas Externas
Puedes leer y analizar un archivo CSV utilizando las funciones incorporadas de Elixir:

```elixir
defmodule LectorCSV do
  def leer_archivo(ruta_archivo) do
    File.stream!(ruta_archivo)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# Uso de muestra
LectorCSV.leer_archivo("datos.csv")
# Salida: [["Encabezado1", "Encabezado2"], ["Valor1Fila1", "Valor2Fila1"], ["Valor1Fila2", "Valor2Fila2"]]
```

### Escribiendo en un Archivo CSV
De manera similar, para escribir datos en un archivo CSV:

```elixir
defmodule EscritorCSV do
  def escribir_en_archivo(ruta_archivo, datos) do
    File.open(ruta_archivo, [:write], fn archivo ->
      Enum.each(datos, fn fila ->
        IO.write(archivo, Enum.join(fila, ",") <> "\n")
      end)
    end)
  end
end

# Uso de muestra
datos = [["Encabezado1", "Encabezado2"], ["Valor1", "Valor2"], ["Valor3", "Valor4"]]
EscritorCSV.escribir_en_archivo("salida.csv", datos)
# Crea salida.csv con los datos formateados como CSV
```

### Usando `nimble_csv`
Para un manejo más complejo de CSV, `nimble_csv` proporciona una manera poderosa y flexible de trabajar con datos CSV. Primero, añade `nimble_csv` a tus dependencias en `mix.exs` y ejecuta `mix deps.get`:

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

Analizando datos CSV con `nimble_csv`:

```elixir
defmodule MiAnalizadorCSV do
  NimbleCSV.define(MiAnalizador, separator: ",", escape: "\\")

  def analizar(ruta_archivo) do
    ruta_archivo
    |> File.stream!()
    |> MiAnalizador.parse_stream()
    |> Enum.to_list()
  end
end

# Uso de muestra
MiAnalizadorCSV.analizar("datos.csv")
# La salida con nimble_csv se puede personalizar según la definición, pero generalmente parece una lista de listas o tuplas dependiendo de cómo configures tu analizador.
```

Escribir datos CSV usando `nimble_csv` requiere transformar manualmente tus datos en un formato adecuado y luego escribirlos en un archivo, muy parecido al ejemplo de Elixir simple pero aprovechando `nimble_csv` para generar filas CSV formateadas correctamente.

Al elegir el enfoque adecuado para la complejidad de tu tarea, puedes manejar archivos CSV en Elixir con gran flexibilidad y poder.
