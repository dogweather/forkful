---
title:                "Javascript: Sprawdzanie, czy istnieje katalog"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w trakcie programowania musimy sprawdzić, czy dany katalog istnieje. Może to być potrzebne w celu uniknięcia błędów lub poprawnego odczytu danych. W tym artykule dowiesz się, jak sprawdzić istnienie katalogu w języku JavaScript.

## Jak to zrobić

Zacznijmy od importowania modułu `fs`, który jest wbudowany w środowisko Node.js. Następnie użyjemy metody `existsSync()` wraz z ścieżką do naszego katalogu jako argumentem.

```Javascript
const fs = require('fs');
fs.existsSync('/sciezka/do/katalogu');
```

Jeśli katalog istnieje, metoda zwróci wartość `true`, a w przeciwnym razie `false`. Możemy również użyć metody `statSync()`, która zwróci błąd, jeśli ścieżka nie istnieje.

```Javscript
const fs = require('fs');
try {
  fs.statSync('/sciezka/do/katalogu');
  console.log('Katalog istnieje!');
}
catch(err) {
  console.error('Katalog nie istnieje!');
}
```

## Deep Dive

W przypadku, gdy chcemy sprawdzić, czy katalog istnieje na zdalnym serwerze, musimy użyć modułu `ssh2-sftp-client`. Ten moduł umożliwia nam połączenie z serwerem SFTP i przeprowadzenie operacji na plikach i katalogach.

Najpierw musimy utworzyć połączenie ustawiając odpowiednie dane uwierzytelniajace oraz hosta i port. Następnie użyjemy metody `exists()` z ścieżką do katalogu jako argumentem, która zwróci wartość `true`, jeśli katalog istnieje, a `false` w przeciwnym razie.

```Javascript
const sftp = require('ssh2-sftp-client');
const config = {
  host: 'hostname',
  port: 'port',
  username: 'login',
  password: 'haslo'
};
let client = new sftp();
client.connect(config)
.then(() => {
  client.exists('/sciezka/do/katalogu');
})
.then((exists) => {
  if (exists) 
    console.log('Katalog istnieje!');
  else
    console.log('Katalog nie istnieje!');
})
.catch((err) => {
  console.error(err);
})
.finally(() => {
  client.end();
});
```

## Zobacz również

- [Dokumentacja Node.js - fs.existsSync()](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Dokumentacja Node.js - fs.statSync()](https://nodejs.org/api/fs.html#fs_fs_statsync_path)
- [Dokumentacja ssh2-sftp-client - exists()](https://ssh2-sftp-client.js.org/#existspath-options)
- [Moduł ssh2-sftp-client na GitHub](https://github.com/jyu213/ssh2-sftp-client)