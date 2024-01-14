---
title:                "C: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

우리는 프로그래밍을 할 때 종종 두 개의 날짜를 비교해야 합니다. 예를 들어 사용자의 생일과 현재 날짜를 비교하거나, 두 개의 이벤트가 언제 일어났는지 비교하려고 할 때가 있습니다. 이러한 날짜 비교는 우리가 코딩을 할 때 자주 사용되는 기술 중 하나입니다. 그렇기 때문에 우리는 두 날짜를 비교하는 방법을 알아야 합니다.

## 어떻게

우선 두 개의 날짜를 나타내는 변수를 설정합니다. 그리고 우리는 `if` 문을 통해 두 날짜가 같은지, 또는 어떤 날짜가 더 큰지를 확인할 수 있습니다. 예를 들어, 먼저 두 변수의 년도를 비교하고, 만약 년도가 같다면 월을 비교하고 다시 만약 월이 같다면 일을 비교하는 식으로 진행할 수 있습니다.

```C
int date1_year = 1995;
int date1_month = 12;
int date1_day = 22;

int date2_year = 2021;
int date2_month = 9;
int date2_day = 27;

if (date1_year > date2_year) {
    printf("Date 1 is later than Date 2");
} else if (date1_year < date2_year) {
    printf("Date 2 is later than Date 1");
} else {
    if (date1_month > date2_month) {
        printf("Date 1 is later than Date 2");
    } else if (date1_month < date2_month) {
        printf("Date 2 is later than Date 1");
    } else {
        if (date1_day > date2_day) {
            printf("Date 1 is later than Date 2");
        } else if (date1_day < date2_day) {
            printf("Date 2 is later than Date 1");
        } else {
            printf("The two dates are the same");
        }
    }
}
```

위의 코드를 실행하면 다음과 같은 결과가 나옵니다.

```C
Date 2 is later than Date 1
```

## 깊게 살펴보기

두 개의 날짜를 비교할 때, 우리는 변수에 직접 날짜를 입력할 수도 있지만, `struct`를 사용하여 좀 더 구조적으로 표현할 수도 있습니다. 예를 들어, 다음과 같이 `struct`를 정의할 수 있습니다.

```C
struct date {
    int year;
    int month;
    int day;
};
```

또는 특정 날짜를 나타내는 변수를 선언할 때 `struct`를 사용할 수도 있습니다.

```C
struct date my_birthday = {1995, 12, 22};
```

또한 복수 개의 날짜를 비교해야 할 때는 배열을 사용할 수도 있습니다.

```C
struct date events[3] = {{2021, 9, 27}, {2021, 11, 3}, {2022, 1, 1}};
```

이렇게 `struct`를 사용하면 우리는 날짜 비교를 더 직관적이고 구조적으로 할 수 있습니다. 따라서 더 깊이 있는 프로그래밍을 할 때 유용하게 사용할 수 있습니다.

## 또 다른 정보

- [C 비교 연산자](https://dojang.io/mod/page/view.php?id=392)
- [C struct 구조체](https://dojang.io/mod/page/view.php?id=391)
- [C 배열](https://dojang.io/mod/page/view.php?id=405)

## 참고

위의 코드와 결과는 예시일 뿐이