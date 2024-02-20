---
date: 2024-01-26 03:50:00.676829-07:00
description: "Att dyka in i en debugger handlar allt om att stega igenom din kod,\
  \ se kugghjulen snurra och f\xE5nga de d\xE4r el\xE4ndiga buggarna p\xE5 bar g\xE4\
  rning. Programmerare\u2026"
lastmod: 2024-02-19 22:04:57.089941
model: gpt-4-0125-preview
summary: "Att dyka in i en debugger handlar allt om att stega igenom din kod, se kugghjulen\
  \ snurra och f\xE5nga de d\xE4r el\xE4ndiga buggarna p\xE5 bar g\xE4rning. Programmerare\u2026"
title: "Att anv\xE4nda en debugger"
---

{{< edit_this_page >}}

## Vad & Varför?
Att dyka in i en debugger handlar allt om att stega igenom din kod, se kugghjulen snurra och fånga de där eländiga buggarna på bar gärning. Programmerare använder debuggerare eftersom de är detektivverktygen som hjälper oss att räkna ut var saker går fel utan att riva av oss håret.

## Hur man gör:
Här är en liten försmak av felsökning i Kotlin med IntelliJ IDEA - Sherlock Holmes bland IDE:er:

```kotlin
fun main() {
    val mysteryNumber = 42
    var guess = 0

    while (guess != mysteryNumber) {
        println("Gissa numret: ")
        guess = readLine()?.toIntOrNull() ?: continue // Ignorera dåliga inmatningar

        // Sätt en brytpunkt här för att se 'guess' i aktion
        if (guess < mysteryNumber) {
            println("För lågt!")
        } else if (guess > mysteryNumber) {
            println("För högt!")
        }
    }

    println("Du fick det! Det mystiska numret var $mysteryNumber")
}
```

Debugger-utdata:
```
Gissa numret: 
10
För lågt!
Gissa numret: 
50
För högt!
Gissa numret: 
42
Du fick det! Det mystiska numret var 42
```

## Djupdykning
Debuggerare har varit med i spelet sedan 50-talet. Då var de ganska primitiva och felsökning kunde handla mer om hårdvara än mjukvara. Numera låter en debugger som den i IntelliJ IDEA oss sätta brytpunkter, stega igenom kod rad för rad och inspektera tillståndet av variabler när vi vill.

Medan IntellJs debugger är superpraktisk för Kotlin, är den inte den enda fisken i havet. Det finns en rad alternativ som Logcat för Android-utveckling, eller kommando-radsverktyg som jdb för minimalisterna. Den underliggande magin här handlar mestadels om JVM Tool Interface (JVMTI), som låter debuggerare interagera med Java Virtual Machine och håller Kotlin-utvecklare i loopen.

## Se också
- Dokumentation för IntelliJ IDEA Debugger: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)
