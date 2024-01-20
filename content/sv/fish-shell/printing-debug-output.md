---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

### Vad & varför?
Att skriva ut felsökningsdata är en teknik som hjälper programmerare att hitta fel och problem i sin kod. Det gör det möjligt för oss att se vad vårt program gör steg för steg och upptäcka var exakt saker går snett.

### Så här gör du:
Här är några exempel på hur man använder `echo` för att skriva ut debugginformation i Fish shell:

```Fish Shell
# Skriv ut en enkel meddelande
echo "Detta är ett felsökningsmeddelande"

# Skriv ut en variabels värde
set variabel "Detta är ett värde"
echo $variabel
```
Och här är vad output ser ut:
```
Detta är ett felsökningsmeddelande
Detta är ett värde
```

### Djupdykning
Den `echo` kommando i Fish shell härstammar från Unix operativsystem, och har sedan dess blivit en standardfunktion i de flesta kommandotolkar. Det finns andra alternativ för att skriva ut debugginformation, till exempel `printf` och `write`, men `echo` är det mest använda på grund av sin enkelhet. I Fish shell, kommer `echo` att skriva till den standard output streamen (`stdout`), vilket innebär att utdata kan omdirigeras till filer eller andra program.

### Se också
För mer information om `echo`, se Fish shell dokumentation: 
https://fishshell.com/docs/current/cmds/echo.html

För instruktioner om hur man omdirigerar output till filer eller andra program, se: 
https://fishshell.com/docs/current/tutorial.html#redirection