---
title:    "C: 미래나 과거의 날짜 계산하기"
keywords: ["C"]
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것이 왜 유용한지에 대해 설명합니다.

일상생활에서 우리는 종종 특정 날짜를 알고 싶을 때가 있습니다. 휴가 계획을 세우거나 중요한 일정을 기록할 때, 우리는 날짜를 계산하고 싶어합니다. 하지만 모든 날짜를 직접 계산하는 것은 번거롭고 실수의 여지가 있습니다. 따라서 프로그래밍을 통해 날짜를 계산하는 것은 매우 유용합니다.

## 어떻게

다양한 방법으로 C 언어를 사용하여 날짜를 계산하는 방법을 설명합니다. 코드 블록 안에서 예제와 함께 실제 출력 결과를 보여줍니다.

```C
// 현재 날짜와 시간을 가져오는 코드 예제
time_t now = time(0); 
struct tm *local_time = localtime(&now); 
```

이 코드는 현재 시스템에서 날짜와 시간을 가져오는 코드입니다. 이를 기반으로 미래나 과거의 날짜를 계산할 수 있습니다.

```C
// 3일 후의 날짜를 출력하는 코드 예제
struct tm future_date = *local_time; 
future_date.tm_mday += 3; 
mktime(&future_date); 

printf("3일 후는 %04d년 %02d월 %02d일입니다.", 
future_date.tm_year+1900, future_date.tm_mon+1, future_date.tm_mday);
```

위 코드 예제에서는 `tm` 구조체를 사용하여 날짜를 계산하고, `mktime` 함수를 사용하여 변경된 날짜를 출력 형식에 맞게 조정합니다. 이를 통해 원하는 날짜를 쉽게 계산하고 출력할 수 있습니다.

## 더 들어가기

날짜를 계산하는 과정에서 자세한 내용을 다룹니다. C 언어에서 날짜와 관련된 다양한 함수들의 사용법과 그 원리를 설명합니다. 또한 다른 언어에서도 비슷한 방식으로 날짜를 계산할 수 있는 방법을 소개합니다. 이를 통해 날짜 계산에 대한 이해도를 높일 수 있습니다.

## 관련 링크

- <https://www.tutorialspoint.com/c_standard_library/time_h.htm>
- <https://en.cppreference.com/w/c/chrono>
- <https://zetawiki.com/wiki/%EC%9D%B8%EC%A6%9D%EC%88%98_%EA%B3%84%EC%82%B0%ED%95%98%EA%B8%B0>
- <https://proglib.io/p/30-c-date-operations/>