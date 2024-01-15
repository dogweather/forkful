---
title:                "Odczytywanie argumentów wiersza polecenia"
html_title:           "TypeScript: Odczytywanie argumentów wiersza polecenia"
simple_title:         "Odczytywanie argumentów wiersza polecenia"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak możesz przekazać dane do swojego programu, gdy działasz z wiersza poleceń? W tym artykule dowiesz się, jak czytać argumenty z wiersza poleceń w języku TypeScript i wykorzystać to w swoich projektach. 

## Jak To Zrobić

W JavaScript, aby uzyskać dostęp do argumentów z wiersza poleceń, można użyć obiektu `process.argv`. W TypeScript, możesz wykorzystać to samo podejście, ponieważ jest to nadal ważna część platformy Node.js. Spójrzmy na poniższy przykład, który pobiera argumenty z wiersza poleceń i wyświetla je jako tablicę.

```TypeScript
const args = process.argv; 
console.log(args); 
```

Gdy uruchomisz ten kod z wiersza poleceń i przekażesz kilka argumentów, zobaczysz je wyświetlone jako tablicę. Na przykład dla polecenia `node index.ts hello world`, otrzymasz następujące wyjście:

``` 
[ "node", "index.ts", "hello", "world" ]
```

Możesz również użyć obiektu `process.argv` w połączeniu z pętlą `for` lub metodą `forEach` do przetwarzania pojedynczych argumentów w pętli. Na przykład:

```TypeScript 
const args = process.argv; 
for(let i = 2; i < args.length; i++){ 
  console.log("Argument #"+(i-1)+": "+ args[i]); 
}
```

W powyższym przykładzie ignorujemy dwa pierwsze elementy tablicy `process.argv`, ponieważ są one zarezerwowane dla ścieżki do pliku i nazwy programu. 

## Głębsza Analiza

W języku TypeScript istnieje również wbudowany moduł `yargs`, który ułatwia przetwarzanie argumentów z wiersza poleceń. Moduł ten zapewnia wiele przydatnych funkcji, takich jak obsługa opcji, domyślne wartości oraz weryfikacja wartości argumentów.

Aby użyć modułu `yargs`, musisz najpierw go zainstalować za pomocą menedżera pakietów - `npm install -g yargs`. Następnie możesz go wykorzystać w swoim programie. Spójrzmy na poniższy przykład, który wykorzystuje moduł `yargs` do obsługi dwóch opcjonalnych argumentów - `name` i `greeting`, z domyślnymi wartościami ustawionymi na "World" i "Hello" odpowiednio.

```TypeScript 
import * as yargs from "yargs"; 

const options = yargs.option("name", {
  alias: "n", 
  describe: "Name of the person", 
  type: "string"
}).option("greeting", { 
  alias: "g", 
  describe: "Greeting message", 
  type: "string" 
}).argv;

const name = options.name || "World"; 
const greeting = options.greeting || "Hello"; 
console.log(greeting+", "+name+"!"); 
```

Jeśli uruchomisz powyższy kod z wiersza poleceń bez przekazywania żadnych opcji, zostanie wyświetlone "Hello, World!". Natomiast dla polecenia `node index.ts --name Joe --greeting Hi`, otrzymasz "Hi, Joe!". 

## Zobacz Również

- [Dokumentacja modułu `yargs`](https://www.npmjs.com/package/yargs)
- [Dokumentacja obiektu `process`](https://nodejs.org/api/process.html)