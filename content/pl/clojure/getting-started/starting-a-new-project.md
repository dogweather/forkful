---
date: 2024-01-20 18:03:18.761321-07:00
description: "Rozpoczynanie nowego projektu to stworzenie podstawy pod przysz\u0142\
  \u0105 aplikacj\u0119. Programi\u015Bci tworz\u0105 nowe projekty, aby rozwi\u0105\
  zywa\u0107 problemy, eksplorowa\u0107 nowe\u2026"
lastmod: '2024-03-13T22:44:34.995531-06:00'
model: gpt-4-1106-preview
summary: "Rozpoczynanie nowego projektu to stworzenie podstawy pod przysz\u0142\u0105\
  \ aplikacj\u0119. Programi\u015Bci tworz\u0105 nowe projekty, aby rozwi\u0105zywa\u0107\
  \ problemy, eksplorowa\u0107 nowe\u2026"
title: Rozpoczynanie nowego projektu
weight: 1
---

## What & Why? | Co i dlaczego?
Rozpoczynanie nowego projektu to stworzenie podstawy pod przyszłą aplikację. Programiści tworzą nowe projekty, aby rozwiązywać problemy, eksplorować nowe pomysły lub uczyć się nowych technik.

## How to: | Jak to zrobić:
Tworzenie nowego projektu w Clojure może być szybkie i proste przy użyciu Leiningen lub Boot. Tutaj skupimy się na Leiningen, najpopularniejszym narzędziu.

1. Zainstaluj Leiningen:
```shell
brew install leiningen # na macOS
```
na Linuxie użyj menedżera pakietów lub skryptu z oficjalnej strony Leiningen.

2. Utwórz nowy projekt:
```shell
lein new app moj-projekt
```

3. Zobacz strukturę projektu:
```shell
tree moj-projekt
```

Output powinien wyglądać mniej więcej tak:
```
moj-projekt
├── project.clj
├── README.md
├── resources
├── src
│   └── moj_projekt
│       └── core.clj
└── test
    └── moj_projekt
        └── core_test.clj
```

4. Uruchom repl i eksperymentuj:
```shell
cd moj-projekt
lein repl
```

W REPLu, możesz teraz załadować swój kod:
```clojure
(require '[moj-projekt.core :as core])
(core/-main) ; Jeśli jest zdefiniowane w core.clj
```

## Deep Dive | W głębi tematu:
Leiningen pojawił się w 2009 roku i szybko stał się standardem w ekosystemie Clojure. Alternatywy jak Boot czy nowsze tools.deps pozwolą na większą elastyczność, ale Leiningen nadal jest dobrym wyborem dla większości projektów dzięki swojemu ekosystemowi pluginów i przyjazności dla początkujących.

Gdy tworzysz projekt, `project.clj` odgrywa kluczową rolę, definiując zależności, pluginy i taski. Clojure, jako język działający na JVM, korzysta z Maven Central i Clojars do zarządzania bibliotekami.

Każdy plik źródłowy w katalogu `src` to nowy namespace, zazwyczaj mapowany 1:1 do struktury katalogów. Dzięki temu zarządzanie i organizacja kodu stają się przejrzyste.

## See Also | Zobacz również:
- Oficjalna strona [Leiningen](https://leiningen.org/)
- Dokumentacja [Clojure](https://clojure.org/guides/getting_started)
- Tutorial [Clojure for the Brave and True](https://www.braveclojure.com/)
- Repozytorium z bibliotekami [Clojars](https://clojars.org/)
- Alternatywny system budowania projektów [Boot](https://boot-clj.com/)
- Guide do nowego narzędzia [tools.deps](https://clojure.org/guides/deps_and_cli)
