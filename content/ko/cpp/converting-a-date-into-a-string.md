---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 날짜를 문자열로 변환하기: C++에서는 어떻게 하나요?

## 무엇이며 왜 필요한가?
날짜를 문자열로 변환하는 것은, 날짜를 표기하는 확정적인 방법을 갖게 되는 것입니다. 프로그래머들이 이를 사용하는 이유는, 사용자 친화적인 형태로 날짜를 표시하거나, 파일 이름을 만드는 등의 기능을 달성하기 위해서입니다.

## 어떻게 하는가: 
```C++
#include <iostream>
#include <chrono>
#include <iomanip>
#include <sstream>

int main() {
    auto now = std::chrono::system_clock::now();
    std::time_t t = std::chrono::system_clock::to_time_t(now);
    std::tm* now_tm = std::localtime(&t);
    std::stringstream ss;
    ss << std::put_time(now_tm, "%Y-%m-%d %H:%M:%S");
    std::string str_now = ss.str();

    std::cout << "Current date and time: " << str_now << '\n';

    return 0;
}
```
이 코드의 결과는 현재 시스템의 날짜와 시간을 "YYYY-MM-DD HH:MM:SS" 형식의 문자열로 출력합니다.

## 깊이 들여다보기
역사적 맥락: C++에서 날짜를 문자열로 변환하는 것은 초기 표준 라이브러리부터 가능했습니다. 그러나 이 기능은 C++11에서 보다 강화되었고 이후 버전에서는 이 가독성이 향상되었습니다.

대안: 최신 버전의 C++에서는 여러 가지 방법으로 날짜를 문자열로 변환할 수 있습니다. std::put_time 함수 외에도, strftime, to_string, fmt 라이브러리 등 다양한 메서드를 사용할 수 있습니다.

구현 세부 사항: 위의 코드에서 사용한 std::put_time은 형식화된 시간을 출력 스트림에 추가하는 함수입니다. 이 함수는 먼저 시간 정보를 지정된 형식의 문자열로 변환한 후, 이 문자열을 출력 스트림에 추가합니다.

## 참고 자료
[CPP 리퍼런스: put_time](https://en.cppreference.com/w/cpp/io/manip/put_time)
[StackOverflow: 날짜를 문자열로 변환하는 방법](https://stackoverflow.com/questions/16357999/current-date-and-time-as-string)