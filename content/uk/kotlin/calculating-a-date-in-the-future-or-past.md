---
title:    "Kotlin: Обчислення дати в майбутньому або минулому"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Чому

Обчислення дати в майбутньому або минулому може бути корисним для планування подій або зберігання даних щодо минулих подій.

## Як

```Kotlin
fun calculateFutureDate(years: Int, months: Int, days: Int): LocalDate {
   val today = LocalDate.now()
   val futureDate = today.plusYears(years).plusMonths(months).plusDays(days)
   return futureDate
}

fun calculatePastDate(years: Int, months: Int, days: Int): LocalDate {
   val today = LocalDate.now()
   val pastDate = today.minusYears(years).minusMonths(months).minusDays(days)
   return pastDate
} 

println("Дата в майбутньому: " + calculateFutureDate(5, 2, 18))
// Output: Дата в майбутньому: 2026-09-18

println("Дата в минулому: " + calculatePastDate(2, 6, 10))
// Output: Дата в минулому: 2017-04-08
```

## Глибше в обчислення

Kotlin пропонує декілька корисних функцій для обчислення дат в майбутньому або минулому. Функція `plusYears()` дозволяє додавати роки до поточної дати, `plusMonths()` - місяці, а `plusDays()` - дні. Аналогічно, функція `minusYears()` виконує обчислення в майбутньому, `minusMonths()` - місяці, а `minusDays()` - дні.

## Дивіться також

- [Офіційна документація Kotlin](https://kotlinlang.org/)
- [Стаття про роботу з датами в Kotlin](https://www.baeldung.com/kotlin/dates)
- [Відео з прикладами обчислення дат в Kotlin](https://www.youtube.com/watch?v=vruGNhGDmns)