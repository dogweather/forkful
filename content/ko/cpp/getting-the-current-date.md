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

# 현재 날짜를 얻는 방법과 그 이유

## 무엇인가요?
현재 날짜를 얻는 것이란, 시스템이나 프로그램이 현재 날짜를 알아내는 것을 말합니다. 프로그래머들은 이 기능을 사용하여 시간 관련 기능을 구현하거나, 로그를 기록하거나, 파일을 만들 때 날짜 정보를 포함시킬 수 있습니다.

## 왜 하나요?
현재 날짜는 프로그램이나 시스템이 언제 작동했는지를 알 수 있는 중요한 정보입니다. 또한 시간 관련 기능을 구현하기 위해 필수적인 요소입니다. 예를 들어, 사용자가 게시물을 작성한 시간을 표시하거나, 파일 이름에 날짜 정보를 포함시켜 다른 파일과 구분할 수 있습니다.

## 방법:
```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
  // 현재 시간을 받아오는 코드
  time_t now = time(0);

  // time_t 변수에서 현재 날짜 정보를 얻는 코드
  char* date = ctime(&now);

  // 출력
  cout << "현재 날짜: " << date << endl;

  return 0;
}
```
위의 코드는 현재 날짜를 출력하는 간단한 예제입니다. <code>ctime</code> 함수를 사용하여 <code>time_t</code> 변수에서 날짜 정보를 얻어오고, <code>cout</code> 함수를 사용하여 출력합니다.

## 더 알아보기:
### 역사적인 배경:
현재 날짜를 얻는 기능은 시스템에 따라 다르게 구현될 수 있습니다. 예전에는 시스템의 내부 시계를 사용해 현재 날짜를 알아내는 방식이었지만, 이제는 인터넷을 통해 정확한 현재 날짜를 서버에서 받아오기도 합니다.

### 대안:
<code>#include <ctime></code> 라이브러리 대신 <code>#include <chrono></code> 라이브러리를 사용하여 현재 날짜를 얻을 수도 있습니다. <code>std::chrono::system_clock::now()</code> 함수를 사용하는 방법이 있습니다.

### 구현 상세:
<code>time_t</code>는 1970년 1월 1일부터 현재까지의 초 단위를 저장하는 데이터 형식입니다. 따라서 <code>time_t</code> 변수에서 날짜 정보를 얻으려면 초 단위를 일 단위로 변환해주어야 합니다. <code>ctime</code> 함수는 <code>time_t</code> 변수를 파라미터로 받아서 문자열 형태의 날짜 정보를 리턴해줍니다.

## 참고 자료:
- [ctime 라이브러리 문서](https://www.cplusplus.com/reference/ctime/)
- [chrono 라이브러리 문서](https://www.cplusplus.com/reference/chrono/)