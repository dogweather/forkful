---
title:                "Ruby: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con CSV en Ruby?

CSV (Comma-Separated Values) son archivos de texto que permiten almacenar y organizar datos de manera tabular. Esto es muy útil para trabajar con grandes cantidades de información, como por ejemplo en el ámbito empresarial. En Ruby, podemos utilizar la librería `CSV` para manipular estos archivos y realizar tareas como importar o exportar datos.

## Cómo hacerlo

**Importar datos desde un archivo CSV**

Podemos importar datos desde un archivo CSV utilizando el método `read` de la clase `CSV`. En este ejemplo, tenemos un archivo `users.csv` con información de usuarios, y queremos almacenarla en un array de hashes.

```Ruby
require 'csv'

users = []

CSV.read('users.csv', headers: true).each do |row|
    users << row.to_hash
end

puts users

# Output:
# [{:name=>"John", :age=>"28", :country=>"USA"}, {:name=>"Maria", :age=>"32", :country=>"Spain"}, {:name=>"Jane", :age=>"25", :country=>"Canada"}]
```

**Exportar datos a un archivo CSV**

También podemos exportar datos a un archivo CSV utilizando el método `open` de la clase `CSV`. En este ejemplo, tenemos un array de hashes con información de productos y queremos exportarla a un archivo `products.csv`.

```Ruby
require 'csv'

products = [
  { name: "Laptop", price: "$1000", brand: "Apple" },
  { name: "Phone", price: "$500", brand: "Samsung" },
  { name: "Headphones", price: "$100", brand: "Sony" }
]

CSV.open('products.csv', 'w') do |csv|
  csv << ["Name", "Price", "Brand"]
  products.each do |product|
    csv << [product[:name], product[:price], product[:brand]]
  end
end

# Output:
# Name,Price,Brand
# Laptop,$1000,Apple
# Phone,$500,Samsung
# Headphones,$100,Sony
```

## Profundizando

La librería `CSV` de Ruby también nos permite realizar otras tareas como modificar datos existentes, ordenarlos, o incluso crear nuevos archivos CSV. Podemos revisar la documentación oficial para obtener más información y descubrir todas las posibilidades que nos ofrece.

## Ver también

* [Documentación oficial de la librería `CSV` de Ruby](https://ruby-doc.org/stdlib-2.5.1/libdoc/csv/rdoc/CSV.html)
* [Tutorial de Ruby: Trabajando con archivos CSV](https://www.rubyguides.com/2018/10/parse-csv-ruby/)