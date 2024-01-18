---
title:                "스트링에서 날짜 추출하기"
html_title:           "C++: 스트링에서 날짜 추출하기"
simple_title:         "스트링에서 날짜 추출하기"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

보통 프로그래밍을 하다보면, 문자열로 된 날짜를 다루는 경우가 종종 있습니다. 이때 우리는 날짜를 string에서 분리해야 할 필요성이 생깁니다. 이러한 과정을 날짜를 문자열에서 파싱한다고 합니다. 프로그래머들이 이것을 하는 이유는 간단합니다. 우리가 다루는 날짜가 다양한 형식으로 표시될 수 있기 때문입니다. 따라서 우리는 이러한 다양한 형식을 프로그래밍적으로 처리할 수 있어야 하기 때문에 이 작업이 필요한 것이죠.

## How to:
우리가 다루는 날짜 형식이 아래와 같은 문자열일 때, 우리는 이를 쉽게 파싱할 수 있도록 몇 가지 코드를 작성해보겠습니다.
```C++
std::string date = "2022/01/15";
int year, month, day;
std::sscanf(date.c_str(), "%d/%d/%d", &year, &month, &day);
```
위 코드의 출력 값은 `year` 변수에는 2022, `month` 변수에는 01, `day` 변수에는 15가 저장됩니다.

이제 우리가 다루는 날짜 형식이 다양한 경우에도 유연하게 파싱할 수 있도록 코드를 작성해보겠습니다.
```C++
std::string date = "January 15, 2022";
int year, month, day;
std::sscanf(date.c_str(), "%*s %d, %d", &year, &day);
month = std::stoi(date.substr(0, date.find(" ")));
```
위 코드의 출력 값은 `year` 변수에는 2022, `month` 변수에는 01, `day` 변수에는 15가 저장됩니다.

## Deep Dive:
과거에는 날짜를 문자열로 표현하는 기능이 없었습니다. 따라서 프로그래머들은 날짜를 숫자로 표현하고 이를 계산하는 방식을 사용했습니다. 하지만 이는 프로그래밍의 복잡성을 증가시키고 실수의 가능성을 높였습니다. 따라서 날짜를 문자열로 파싱하는 방식이 등장하게 되었고, 현재에 이르러서는 날짜를 다루는 데 있어 이 방식이 가장 널리 사용되고 있습니다.

날짜를 문자열에서 파싱하는 또 다른 방법으로는 정규식을 사용하는 것이 있습니다. 정규식을 이용하면 더욱 다양한 날짜 형식을 처리할 수 있습니다. 하지만 정규식 자체가 복잡하고 속도가 느린 단점이 있기 때문에 메모리나 실행 속도를 고려해야 합니다.

날짜를 문자열에서 파싱할 때 정확한 결과를 얻기 위해서는 날짜 형식이 명확해야 합니다. 그렇지 않으면 코드가 의도와 다른 결과를 낼 수 있습니다. 따라서 프로그램 내에서 날짜 형식을 미리 지정해놓고 이를 기준으로 파싱하는 것이 중요합니다.

## See Also:
- [`sscanf()` 함수 문서](https://en.cppreference.com/w/c/io/fscanf)
- [정규식 테스트 사이트](https://regexr.com/)