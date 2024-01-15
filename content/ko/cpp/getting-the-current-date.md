---
title:                "현재 날짜 가져오기"
html_title:           "C++: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜?

코딩을 하는 많은 사람들에게 대부분의 프로세스에서 현재 날짜를 찾는 것이 필수적입니다. 예를 들어, 어떤 작업을 수행하고 해당 결과를 보관하거나 프로그램에서 날짜별로 파일이나 데이터를 정리할 때 현재 날짜를 사용할 수 있습니다.

## 하우 투 (How To)

그럼, C++에서 현재 날짜를 어떻게 가져오는지 알아보겠습니다. 우선, 다음과 같이 헤더 파일 &#60;ctime&#62; 을 포함해야 합니다.

```C++
#include &#60;ctime&#62;
```

그리고, 아래의 코드를 사용해서 현재 날짜를 얻을 수 있습니다.

```C++
// 현재 시간(현재 날짜 및 시간)을 저장하는 tm 구조체 생성
time_t now = time(0);

// 현재 날짜 및 시간으로 변환
char* dt = ctime(&now);

// 현재 날짜만 추출
cout << "현재 날짜: "<< dt << endl;
```

위의 코드를 실행하면 다음과 같은 출력 결과를 얻게 될 것입니다.

```
현재 날짜: Tue Nov 9 21:56:04 2021
```

## 딥 다이브 (Deep Dive)

위의 코드에서 사용된 함수인 `time()` 함수는 시간을 지정된 형식으로 리턴해주는 C++의 표준 라이브러리 함수입니다. 이 함수는 파라미터를 받지 않고, 호출 시 현재 시간을 `time_t` 타입으로 리턴합니다.

`time_t` 타입은 시간을 표현하는 클래스이며, 이를 `tm` 구조체에 저장하면 현재 날짜 및 시간 정보를 얻을 수 있습니다. `ctime()` 함수는 `time_t` 변수를 파라미터로 받아서 날짜 및 시간을 문자열 형식으로 표현해줍니다.

## 더 알아보기

- [C++ 날짜 및 시간 함수](https://www.cplusplus.com/reference/ctime/)
- [C++의 time, tm, ctime 함수 이해하기 (영문)](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)

## 참고자료

- [C++ Reference - ctime](https://en.cppreference.com/w/cpp/chrono/c/ctime)
- [C++ 사용자와 개발자를 위한 C++ 표준 라이브러리 출시](https://docs.microsoft.com/ko-kr/cpp/standard-library/release-notes?view=msvc-160)