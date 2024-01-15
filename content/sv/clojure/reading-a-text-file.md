---
title:                "Läsning av en textfil"
html_title:           "Clojure: Läsning av en textfil"
simple_title:         "Läsning av en textfil"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att kunna läsa och bearbeta textfiler är en viktig färdighet inom programmering, särskilt när det gäller att hantera stora datamängder eller samverka med andra program. I detta artikel kommer du lära dig hur du på ett enkelt sätt kan läsa en textfil i Clojure och sedan använda den data som finns i filen för att lösa olika förhandsproblem.

## Hur man gör

Att läsa en textfil i Clojure är en relativt enkel process. Först behöver vi definiera en filväg där vår textfil finns lagrad. Detta kan göras med hjälp av funktionen "file" som tar filvägen som ett argument.

```Clojure
(def filväg (file "minkod.txt"))
```

Vi kan nu använda funktionen "with-open" för att öppna filen och läsa dess innehåll. Detta garanterar att filen stängs automatiskt efter att vi har använt den. Sedan använder vi funktionen "read-line" för att läsa en rad i taget och "println" för att skriva ut resultatet till terminalen.

```Clojure
(with-open [fil (reader filväg)]
  (doseq [rad (line-seq fil)]
    (println rad)))
```

Sample Output:
```
Hej!
Det här är en textfil.
Den innehåller några rader text.
```

För att lagra datan från filen i en variabel kan vi använda funktionen "slurp" som läser hela filen som en sträng.

```Clojure
(def innehåll (slurp filväg))
```

Sample Output:
```
"Hej!\nDet här är en textfil.\nDen innehåller några rader text."
```

## Djupdykning

När du läser en textfil i Clojure är det viktigt att vara medveten om att funktionerna "read-line" och "slurp" tolkar filinnehållet som en sekvens av tecken snarare än rader. Detta betyder att om filen innehåller text med flera rader, kommer funktionerna att läsa in hela texten som en enda rad. För att undvika detta kan du använda funktionen "line-seq" som delar upp strängen vid varje radbrytning och returnerar en sekvens av rader. Det är också viktigt att filen är kodad enligt rätt teckenuppsättning för att undvika problem med specialtecken och icke-standardtecken.

Om du vill läsa en textfil som finns på en annan plats än din lokala dator kan du använda funktionen "slurp-url" som tar URL-adressen som argument.

```Clojure
(def remote-innehåll (slurp-url "https://dinadress.com/textfil.txt"))
```

## Se även

- [Officiell Clojure dokumentation](https://clojuredocs.org/)
- [Official Clojure YouTube channel](https://www.youtube.com/user/clojuretv/videos)
- [Clojure Subreddit](https://www.reddit.com/r/Clojure/)