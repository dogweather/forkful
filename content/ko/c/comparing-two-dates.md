---
title:                "C: 두 날짜 비교"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜 비교를 하는 이유는 무엇일까요? 프로그램을 만들거나 데이터를 분석할 때, 날짜가 중요한 역할을 합니다. 따라서 두 날짜를 비교하는 것은 필수적입니다.

## 하는 방법

C 프로그래밍에서는 두 날짜를 비교하는 것이 간단합니다. 아래 코드를 참고해보세요.

```C
#include <stdio.h>

int main() {
	// 날짜 비교를 위한 변수 설정
	int year1 = 2021;
	int month1 = 9;
	int day1 = 1;
	int year2 = 2020;
	int month2 = 8;
	int day2 = 31;

	// 첫 번째 날짜가 더 큰 경우
	if (year1 > year2 || (year1 == year2 && month1 > month2) || (year1 == year2 && month1 == month2 && day1 > day2)) {
		printf("%d년 %d월 %d일이 %d년 %d월 %d일보다 더 큽니다.\n", year1, month1, day1, year2, month2, day2);
	}
	// 두 번째 날짜가 더 큰 경우
	else if (year2 > year1 || (year2 == year1 && month2 > month1) || (year2 == year1 && month2 == month1 && day2 > day1)) {
		printf("%d년 %d월 %d일이 %d년 %d월 %d일보다 더 큽니다.\n", year2, month2, day2, year1, month1, day1);
	}
	// 두 날짜가 같은 경우
	else {
		printf("%d년 %d월 %d일과 %d년 %d월 %d일은 같은 날짜입니다.\n", year1, month1, day1, year2, month2, day2);
	}

	return 0;
}
```

위 코드를 실행하면 다음과 같은 결과가 나옵니다.

```
2021년 9월 1일이 2020년 8월 31일보다 더 큽니다.
```

이를 통해 두 날짜를 비교하는 방법을 알 수 있습니다. 단순히 년, 월, 일이 더 큰지 작은지 비교하는 것으로도 충분합니다.

## 깊이 들어가기

C 언어에서는 날짜를 비교하기 위해 비교 연산자인 `>`, `<`, `==`를 사용합니다. 그리고 논리 연산자인 `&&`를 사용하여 년, 월, 일이 모두 같아야 동일한 날짜로 판단하도록 할 수 있습니다. 이러한 기본 개념 외에도 다양한 방법으로 날짜를 비교할 수 있으며, 프로그램에 따라 적합한 방법을 선택할 수 있습니다.

## 관련 자료

날짜 비교에 대한 더 자세한 내용은 아래 링크를 참고해보세요.

- [C 프로그래밍 날짜/시간](https://dojang.io/mod/page/view.php?id=346)
- [Compare Two Dates in C with Examples](https://www.sanfoundry.com/c-programming-examples-comparing-two-dates/)
- [C 언어 기초 - 날짜와 시간 다루기](https://shfun.tistory.com/593)
- [The Basics of Date Comparison in C](https://www.techwalla.com/articles/the-basics-of-date-comparison-in-c)