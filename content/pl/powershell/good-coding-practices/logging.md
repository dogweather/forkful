---
title:                "Rejestrowanie zdarzeń"
aliases: - /pl/powershell/logging.md
date:                  2024-01-26T01:07:26.915567-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rejestrowanie zdarzeń"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/logging.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Logowanie polega po prostu na pozostawianiu śladu w kodzie – to sposób na śledzenie tego, co się dzieje, gdy skrypt jest uruchamiany "na żywo". Programiści logują, aby debugować, śledzić zachowanie aplikacji, monitorować wydajność i mieć oko na wszelkie nieprawidłowości.

## Jak to zrobić:
Oto krótki przewodnik, jak dodać podstawowe logowanie do swoich skryptów:

```PowerShell
# Tworzenie prostej wiadomości w logu
Write-Host "Info: Rozpoczynanie procesu skryptu."

# Zapis do pliku
"Info: To jest zalogowana wiadomość." | Out-File -Append myLog.log

# Użycie wbudowanego polecenia cmdlet dla bardziej szczegółowego logowania
Start-Transcript -Path "./detailedLog.log"
Write-Output "Uwaga: Coś nie jest do końca w porządku."
# ... twój skrypt coś robi
Stop-Transcript

# Zawartość detailedLog.log
******************************
Dziennik Windows PowerShell start
Czas startu: 20230324112347
Nazwa użytkownika: PShellGuru@example.com
RunAs User: PShellGuru@example.com
Nazwa konfiguracji: 
Komputer: PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
Aplikacja hostująca: C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
ID procesu: 2024
Wersja PS: 7.1.2
```

Teraz, w twoich logach znajduje się opis tego, co robił twój kod.

## W głąb tematu:
Historycznie, logowanie jest prawie tak stare jak samo programowanie. To jak dziennik kapitana, ale dla oprogramowania. Kiedyś mogły to być wydruki czy maszyny teletypowe; obecnie chodzi o pliki i zaawansowane systemy zarządzania logami.

Gdy jesteś zanurzony w PowerShellu, `Write-Host` jest szybkie i proste, ale wypisuje tylko tekst na konsolę, co nie jest najlepsze dla przechowywania rekordów. `Out-File` daje prosty sposób na wrzucenie tekstu do pliku, ale dla prawdziwej substancji chcesz użyć `Start-Transcript` i `Stop-Transcript`, które logują wszystko—wprowadzone polecenia, wyniki, całość.

Alternatywy? Jasne, jeśli zajmujesz się przedsiębiorstwem, możesz spojrzeć na Dziennik Zdarzeń Windows lub użyć oprogramowania takiego jak Logstash, ale na co dzień, przy swoich skryptach, lepiej trzymać się narzędzi PowerShell. Jeśli chodzi o implementację, pamiętaj, aby logować mądrze – za mało i jest bezużyteczne, za dużo i staje się białym szumem.

## Zobacz również:
Sprawdź to, aby lepiej zrozumieć wszystko, co wiąże się z logowaniem w PowerShellu:
