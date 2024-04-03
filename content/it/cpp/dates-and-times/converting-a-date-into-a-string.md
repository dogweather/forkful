---
date: 2024-01-20 17:36:26.236342-07:00
description: "Convertire una data in una stringa significa trasformarla da un formato,\
  \ tipicamente numerico, a una rappresentazione testuale. I programmatori lo fanno\u2026"
lastmod: '2024-03-13T22:44:43.738650-06:00'
model: gpt-4-1106-preview
summary: Convertire una data in una stringa significa trasformarla da un formato,
  tipicamente numerico, a una rappresentazione testuale.
title: Conversione di una data in una stringa
weight: 28
---

## How to:
```cpp
#include <iostream>
#include <iomanip>
#include <sstream>
#include <chrono>

int main() {
    using namespace std::chrono;
    auto now = system_clock::now();
    time_t t_now = system_clock::to_time_t(now);
    tm utc_tm = *gmtime(&t_now);

    std::stringstream ss;
    ss << std::put_time(&utc_tm, "%Y-%m-%d %H:%M:%S"); // Formato ISO 8601
    std::string date_str = ss.str();

    std::cout << "Data corrente (UTC): " << date_str << std::endl;
    
    return 0;
}
```
Output: `Data corrente (UTC): 2023-04-07 15:45:12`

## Deep Dive:
Storicamente, la gestione del tempo in C++ era abbastanza rudimentale. Solo con C++11 è stata introdotta la libreria `<chrono>`, che ha portato un'astrazione moderna al tempo e alla data. Prima si usavano le strutture `time_t` e `tm` di C, che sono ancora utili quando si convertono le date in stringhe, specie in combinazione con `<iomanip>` per formattare.

Un'alternativa moderna è usare la libreria `fmt` (ora inclusa nella standard library come `<format>` da C++20 in poi), che semplifica ulteriormente la formattazione delle date.

Dettagli importanti includono la gestione del fuso orario (UTC nel nostro esempio) e l'adozione di formati standard come ISO 8601 per l'interoperabilità.

## See Also:
- Documentazione ufficiale di C++ `<chrono>`: https://en.cppreference.com/w/cpp/header/chrono
- Informazioni su `<iomanip>` e `std::put_time`: https://en.cppreference.com/w/cpp/io/manip/put_time
- Tutorial su C++ Date and Time (prima di C++11): https://www.cplusplus.com/reference/ctime/
- Guida all'uso di `<format>` in C++20: https://en.cppreference.com/w/cpp/compiler_support
