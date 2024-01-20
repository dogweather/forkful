---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Konvertering av dato til streng i C++
## Hva og hvorfor?
I programmering er konvertering av dato til streng simpelthen å forevkle datoen i en lesbar tekststreng. Denne prosessen gjør det enklere for brukere å forstå og samhandle med datoer på deres naturlige språk.

## Hvordan gjøre det
For å gjøre dette i moderne C++, bruker vi `<chrono>` og `<iomanip>` biblioteker. 

```C++
#include <iostream>
#include <iomanip>
#include <chrono>

int main() {
    auto now = std::chrono::system_clock::now();
    std::time_t time_now = std::chrono::system_clock::to_time_t(now);
    
    std::cout << std::put_time(std::localtime(&time_now), "%Y-%m-%d %X") << std::endl;
    return 0;
}
```

Utdatamønstret "%Y-%m-%d %X" representerer året-måneden-dagen og tiden. Iført denne koden vil gi deg gjeldende dato og tid, for eksempel: 2020-04-30 13:55:26
     
## Dyp dykk
Å konvertere en dato til en streng var ikke alltid så rett fram i C++. Tidligere måtte vi bruke eldre biblioteker som `<time.h>` eller C-style funksjoner, som ikke stemmer overens med moderne C++ prinsipper. Med introduksjonen av `<chrono>` i C++11, ble det enklere å håndtere dato og tid på en trygg og konsistent måte. 

Alternativt kan du bruke eksterne biblioteker som "date.h" eller "boost/date_time". Men det er alltid best å holde seg til standardbiblioteket når det er mulig. 

Når det gjelder implementasjonsdetaljer, bruker koden først `system_clock::now()` til å hente nåværende tidspunkt. Den konverterer deretter dette til en `time_t` verdi, som kan håndteres av funksjonen `put_time()`. Merk at funksjonen `localtime()` brukes for å konvertere tidspunktet til en struktur som kan brukes av `put_time()`.

## Se også
1. `<chrono>` dokumentasjon: http://en.cppreference.com/w/cpp/header/chrono
2. `<iomanip>` dokumentasjon: http://en.cppreference.com/w/cpp/header/iomanip
3. "date.h" biblioteket: https://github.com/HowardHinnant/date
4. Boost.Date_Time biblioteket: https://www.boost.org/doc/libs/1_53_0/doc/html/date_time.html