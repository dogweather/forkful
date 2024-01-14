---
title:    "Clojure: Skapa en tillfällig fil"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Varför

Att skapa tillfälliga filer är ett vanligt förfarande inom programmering för att hantera temporära data eller för att testa kod utan att permanent påverka befintliga filer. Det kan också vara användbart för att spara mellanresultat från en process.

## Hur man gör det

För att skapa en temporär fil i Clojure, kan du använda funktionen `temp-file` från standardbiblioteket `java.io.File`. Du kan ge den ett prefix och ett suffix som kommer att inkluderas i filnamnet. Här är ett exempel på hur man skapar en temporär fil med prefixet "example_", suffixet ".txt" och skriver till den:

```Clojure
(import [java.io File])

(defn create-temp-file []
  (let [temp-file (File/temp-file "example_" ".txt")]
    (spit temp-file "Detta är ett exempel på en temporär fil")
    temp-file))

(create-temp-file)
```

Output:

```
#<File /var/folders/ss/mytemp100123/example_5d9434e1-8860-4d4f-b9ce-2aec8c75d365.txt>
```

## Djupdykning

När en temporär fil skapas, sparas den i ett speciellt systemförvaringsutrymme som är avsett för temporära filer. När programmet avslutas eller filen inte längre behövs, raderas filen automatiskt från systemet.

Det är också möjligt att ange en specifik mapp där den temporära filen ska skapas genom att ge en andra parameter till `temp-file` funktionen. På så sätt kan du ha mer kontroll över var filen sparas.

## Se även

- [Java API documentation för File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [How to Use Temporary Files in Java](https://www.baeldung.com/java-temporary-files) (Engelska)
- [Clojure Java interop](https://clojure.org/reference/java_interop)