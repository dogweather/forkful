---
title:                "정규 표현식 사용"
html_title:           "C++: 정규 표현식 사용"
simple_title:         "정규 표현식 사용"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?
정규식을 사용하는 것은 문자열 패턴을 찾고 매칭시키는 강력한 도구입니다. 프로그래머들은 이를 통해 데이터 처리, 문자열 검색, 유효성 검사 등에 유용하게 활용할 수 있습니다. 

# 하는 법:
```C++
#include <iostream>
#include <regex>
using namespace std;

int main(){
    // 문자열 생성
    string data = "Hello, my name is John.";
    
    // 정규식 사용하여 매칭
    regex re("John");
    
    // data에서 패턴 매칭
    if(regex_search(data, re)){
        cout << "John을 찾았습니다." << endl;
    } else{
        cout << "John이 없습니다." << endl;
    }
    
    return 0;
}
```
**출력 결과:**
```C++
John을 찾았습니다.
```

# 깊은 곳:
정규식은 1950년대부터 사용되고 있으며, 당시 새로운 컴퓨터 언어로써 소개되었습니다. 대부분의 프로그래밍 언어에 정규식 라이브러리가 내장되어 있지만, 알고리즘의 효율성이 좋지 않을 수 있습니다. 따라서 대안으로 문자열 메소드를 사용할 수도 있습니다. 

# 참고:
- [C++ regex library](https://www.cplusplus.com/reference/regex/)
- [History of Regular Expressions](https://www.regular-expressions.info/history.html)