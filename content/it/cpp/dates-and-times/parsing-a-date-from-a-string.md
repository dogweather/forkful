---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:45.751827-07:00
description: "Come fare: Nel C++ moderno, \xE8 possibile utilizzare la libreria `<chrono>`\
  \ per gestire date e orari nativamente, ma questa non supporta direttamente\u2026"
lastmod: '2024-03-13T22:44:43.736111-06:00'
model: gpt-4-0125-preview
summary: "Nel C++ moderno, \xE8 possibile utilizzare la libreria `<chrono>` per gestire\
  \ date e orari nativamente, ma questa non supporta direttamente l'analisi da stringhe\
  \ senza un parsing manuale per formati pi\xF9 complessi."
title: Analisi di una data da una stringa
weight: 30
---

## Come fare:
Nel C++ moderno, è possibile utilizzare la libreria `<chrono>` per gestire date e orari nativamente, ma questa non supporta direttamente l'analisi da stringhe senza un parsing manuale per formati più complessi. Tuttavia, per i formati di data ISO 8601 e formati personalizzati semplici, ecco come è possibile eseguire il parsing.

**Utilizzando `<chrono>` e `<sstream>`:**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // formato ISO 8601
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day parsed_date;
    iss >> std::chrono::parse("%F", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Data analizzata: " << parsed_date << std::endl;
    } else {
        std::cout << "Analisi della data fallita." << std::endl;
    }
    
    return 0;
}
```
Esempio di output:
```
Data analizzata: 2023-04-15
```

Per formati più complessi o quando si lavora con versioni più vecchie di C++, le librerie di terze parti come `date.h` (la libreria di date di Howard Hinnant) sono popolari. Ecco come è possibile analizzare vari formati con essa:

**Utilizzando la Libreria `date.h`:**
Assicurati di avere la libreria installata. Puoi trovarla [qui](https://github.com/HowardHinnant/date).

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "April 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days parsed_date;
    iss >> date::parse("%B %d, %Y", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Data analizzata: " << parsed_date << std::endl;
    } else {
        std::cout << "Analisi della data da stringa fallita." << std::endl;
    }

    return 0;
}
```
Esempio di output (può variare a seconda delle impostazioni locali e della data del tuo sistema):
```
Data analizzata: 2023-04-15
```
