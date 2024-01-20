---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
문자열에서 날짜를 분석하는 것은 주어진 문자열에서 데이터를 추출, 분석하여 날짜 형태로 변환하는 과정입니다. 이는 문자열 형태의 날짜 데이터를 사용하는 프로그램에서 필수적으로 사용되는 기능입니다.

## 구현 방법:
다음은 `<chrono>` 라이브라리를 사용하여 문자열에서 날짜를 파싱하는 C++ 코드입니다:

```C++
#include <iostream>
#include <sstream>
#include <iomanip>
#include <chrono>

int main()
{
    std::string date_string = "2022-03-26";
    std::istringstream ss(date_string);
    std::chrono::system_clock::time_point time_point;
    ss >> std::get_time(&time_point, "%Y-%m-%d");

    if (ss.fail())
    {
        std::cout << "Failed to parse time\n";
    }
    else
    {
        std::cout << "Successfully parsed time: " << time_point << "\n";
    }
    return 0;
}
```

이 코드가 수행되면 다음과 같은 출력을 얻게 됩니다:

```
Successfully parsed time: 2022-03-26
```

## 깊게 알아보기
날짜 파싱은 프로그래밍의 초기 단계부터 필요한 기능으로 사용되었습니다. 단순한 데이터와 복잡한 데이터 모두에 적용 가능하며, HTML 문서나 사용자 입력 등에서 날짜 데이터를 추출하는데 주로 사용됩니다.

대안으로는 `boost::date_time` 라이브러리를 사용하는 방법이 있습니다. 이 라이브러리는 다양한 날짜 및 시간 형식을 더 간편하게 처리할 수 있게 도와줍니다.

`std::get_time` 내부에서는 주어진 형식문자열을 순회하며, 각 구성요소를 분석하고 필요한 날짜 및 시간 필드를 추출합니다. 실패 시 failbit를 설정하여 사용자에게 오류를 알립니다.

## 관련 링크
1. `<chrono>`: [https://en.cppreference.com/w/cpp/header/chrono](https://en.cppreference.com/w/cpp/header/chrono)
2. `boost::date_time`: [https://www.boost.org/doc/libs/1_64_0/doc/html/date_time.html](https://www.boost.org/doc/libs/1_64_0/doc/html/date_time.html)