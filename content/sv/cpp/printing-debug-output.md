---
title:    "C++: Utskrift av felsökningsresultat"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

Varför: Utskrift av debug-utdata är en viktig del av programmering eftersom den hjälper dig att felsöka och hitta fel i din kod.

Så här: För att skriva ut debug-utdata i C++ kan du använda funktionen "cout" från standard biblioteket i programmet. Detta gör det enkelt att skriva ut variabler och värden som du vill undersöka. Se nedan för ett enkelt exempel:

```C++
int main() {
  int num1 = 5;
  int num2 = 10;
  
  // Skriv ut värdena av num1 och num2
  cout << "Värdena av num1 och num2 är: " << num1 << " och " << num2 << endl;
  
  return 0;
}
```

Utdata från programmet ovan kommer att vara:

```
Värdena av num1 och num2 är: 5 och 10
```

Detta är ett enkelt sätt att använda "cout" för att skriva ut värden i ditt program och undersöka dem för eventuella fel. Du kan också använda olika formateringsalternativ för att skriva ut värden mer lättläst, som att använda "setw" för att justera bredden på utdata.

Djupdykning: När du arbetar med mer komplexa program, kan det vara användbart att kunna skriva ut informationsmeddelanden för att hjälpa dig att spåra fel och hitta buggar. Du kan till exempel använda en kombination av "cout" och "cerr" för att skriva ut meddelanden till terminalen och felmeddelanden till felutdata. Du kan också använda "assert" för att skriva ut meddelanden och pausa programmet vid en viss punkt för att undersöka vad som händer i koden. Det är viktigt att använda debug-utskrifter på ett effektivt sätt och bara lämna dem kvar i din kod under utvecklingsfasen.

Se också: För mer information om debug-utdata i C++ och andra användbara verktyg för felsökning, kan du kolla in följande länkar:

- https://www.tutorialspoint.com/cplusplus/cpp_debugging_techniques.htm
- https://www.geeksforgeeks.org/debugging-c-cpp-code-using-visual-studio/ 
- https://www.learncpp.com/cpp-tutorial/using-debugging-tools/ 

Se även dessa verktyg som kan hjälpa dig att förbättra din felsökningsprocess och utveckla bättre kod. Ha kul med att utforska och använda debug-utdata för att förbättra din programmeringsfärdighet!