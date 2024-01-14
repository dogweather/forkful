---
title:    "TypeScript: Zaczynając nowy projekt"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego warto rozpocząć nowy projekt?

Rozpoczęcie nowego projektu może być ekscytującym wyzwaniem dla każdego programisty. Nie tylko daje możliwość używania nowych technologii i rozwiązań, ale także pozwala na rozwijanie swoich umiejętności. Ponadto, tworzenie nowego projektu może być świetną okazją do pokazania swoich umiejętności swoim potencjalnym pracodawcom lub klientom.

## Jak zacząć?

Pierwszym krokiem w rozpoczęciu nowego projektu w TypeScript jest oczywiście utworzenie nowego pliku projektu. Aby to zrobić, należy przejść do wybranego katalogu i wykonać następujące polecenia w terminalu:

```TypeScript
npm init
```

Następnie należy zainstalować TypeScript globalnie, aby móc korzystać z niego w dowolnym miejscu na komputerze:

```TypeScript
npm install -g typescript
```

Teraz można utworzyć plik `tsconfig.json`, który będzie zawierał konfigurację dla naszego projektu. Wprowadzając polecenie `tsc --init`, zostanie wygenerowany domyślny plik konfiguracyjny. W tym pliku można określić ustawienia takie jak wersja TypeScript, docelowa wersja ECMAScript oraz ścieżki do plików źródłowych i wynikowych.

Następnie należy utworzyć plik `index.ts`, który będzie zawierał nasz kod w TypeScript. Aby skompilować go na kod JavaScript, wystarczy użyć polecenia `tsc` w terminalu:

```TypeScript
tsc
```

Kod zostanie skompilowany do pliku `index.js`, który można uruchomić za pomocą polecenia `node index.js`.

## Deep Dive

Podczas rozpoczynania nowego projektu warto pamiętać o kilku ważnych aspektach, aby projekt był dobrze zorganizowany i łatwy do rozwijania.

Po pierwsze, warto zacząć od określenia celów i wymagań projektu. Dobrze zdefiniowane cele umożliwią efektywniejsze planowanie i projektowanie kodu.

Kolejnym ważnym aspektem jest wybór odpowiednich narzędzi i technologii. W przypadku TypeScript, można rozważyć użycie narzędzi wspomagających jak np. `tslint` lub `ts-node`, które ułatwią pracę z tym językiem.

Nie można również zapominać o testowaniu kodu. W TypeScript można korzystać z narzędzi takich jak `Jest` czy `Mocha` w celu automatycznego testowania aplikacji.

## Zobacz również

- [Dokumentacja TypeScript](https://www.typescriptlang.org/docs/)
- [TypeScript dla początkujących](https://mislavjuracic.com/typescript-for-newbies/)
- [10 powodów dla których warto używać TypeScript](https://www.webdesigndevelopment.ca/10-reasons-use-typescript/)