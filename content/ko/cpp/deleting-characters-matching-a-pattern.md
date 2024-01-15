---
title:                "패턴에 일치하는 문자 삭제하기"
html_title:           "C++: 패턴에 일치하는 문자 삭제하기"
simple_title:         "패턴에 일치하는 문자 삭제하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

어떤 *이유로* 패턴과 일치하는 문자를 삭제하는 작업을 하는지에 대해 최대 2문장으로 설명합니다.

## 방법

"```C++ 
#include <iostream> 
#include <string> 
using namespace std; 

// 패턴 매칭 함수 
string delPattern(string str, char pattern) 
{ 
    // 새로운 문자열 객체 생성 
    string newStr = ""; 

    // 원본 문자열을 반복하면서 패턴과 일치하지 않는 문자를 새로운 문자열에 추가 
    for (int i = 0; i < str.length(); i++) 
    { 
        if (str[i] != pattern) 
        { 
            newStr += str[i]; 
        } 
    } 

    // 최종적으로 새로운 문자열 반환 
    return newStr; 
} 

int main() 
{ 
    // 원본 문자열과 삭제할 패턴 설정 
    string str = "hello world!"; 
    char pattern = 'o'; 

    // 패턴을 삭제한 결과 출력 
    cout << delPattern(str, pattern) << endl; 

    return 0; 
} 
```"

출력:
hell wrld!

## 딥 다이브

패턴과 일치하는 문자를 삭제하는 작업은 문자열 처리에 있어서 매우 유용합니다. 이를 통해 원하는 형식으로 문자열을 조작하거나 특정 문자를 제거할 수 있습니다. 또한 이를 응용하여 더 복잡한 패턴 매칭 알고리즘을 구현할 수도 있습니다.

## 관련 자료들
- [C++ 문자열 처리 기본](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [문자열 처리 함수 정리](https://www.cplusplus.com/reference/string/string/)
- [패턴 매칭 알고리즘에 대한 개념 설명](https://www.geeksforgeeks.org/pattern-matching-implementation-in-c/#:~:text=Pattern%20matching%20is%20the%20process,two%20strings%20'pattern'%20%26%20'text'.)