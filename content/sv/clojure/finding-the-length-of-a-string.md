---
title:    "Clojure: Att hitta längden på en sträng"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

Att hitta längden på en sträng med Clojure

## Varför
Att hitta längden på en sträng kan vara en vanlig uppgift vid programmering. Det kan vara användbart för att bearbeta data eller för att kontrollera giltigheten av en inmatad sträng. I denna artikel kommer vi att titta närmare på hur du kan hitta längden på en sträng med hjälp av Clojure.

## Hur man gör det
För att hitta längden på en sträng i Clojure kan du använda funktionen "count". Denna funktion tar en sträng som argument och returnerar längden på strängen som en heltalsvärde.

```Clojure
(count "Hej! Det här är en sträng.")
```

Detta kodblock kommer att returnera värdet "27", vilket är längden på den givna strängen.

Om du vill kontrollera längden på en sträng som är lagrad i en variabel, kan du använda funktionen "count" på följande sätt:

```Clojure
(def sträng "Detta är en annan sträng.")

(count sträng)
```

Detta kommer att ge samma resultat som tidigare, "27".

Det är också möjligt att hitta längden på en sträng bestående av flera ord genom att använda funktionen "str".

```Clojure
(count (str "Detta" "är" "en" "sträng"))
```

Detta kommer att returnera värdet "15", vilket är längden på den sammanslagna strängen.

## Djupdykning
Förutom att använda funktionen "count" finns det andra sätt att hitta längden på en sträng i Clojure. En annan metod är att använda "seq" vilket returnerar en sekvens av tecken i en sträng. Sedan kan du använda funktionen "into" för att placera dessa tecken i en "set" och sedan ta längden på detta set.

```Clojure
(def sträng "Clojure är ett spännande programspråk.")

(count (into #{} (seq sträng)))
```

Denna metod returnerar också längden på strängen, i detta fall "33".

## Se även
- [Clojure.org](https://clojure.org/)
- [Funktionen "count" i Clojure](https://clojuredocs.org/clojure.core/count)
- [Djupdykning i Clojure](https://www.clojure.org/about/functions#seq)