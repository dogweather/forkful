---
title:                "문자열 대문자화"
html_title:           "C++: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 무엇 & 왜? 
문자열 대문자화란 무엇인지 설명하고, 프로그래머들이 왜 이렇게 하는지에 대해 간단히 설명합니다.

먼저, 문자열 대문자화란 문자열의 모든 문자를 대문자로 바꾸는 것을 말합니다. 이는 보통 입력받은 문자열의 형식을 통일시키기 위해서 혹은 검색 기능을 구현할 때 대소문자 구분을 없애기 위해서 사용됩니다. 

## 방법:
아래 코드 블록을 참고하여, C++ 언어를 사용하여 문자열을 대문자로 변환하는 방법을 소개합니다. 코드 블록 안에 있는 샘플 코드를 참고하여, 실제 출력 결과를 확인할 수 있습니다.

```C++
// 입력받은 문자열을 대문자로 변환하는 함수
string capitalize(string str) {
    
    // 문자열의 길이만큼 반복
    for(int i=0; i<str.length(); i++) {
        
        // 현재 문자가 소문자라면
        if(str[i] >= 'a' && str[i] <= 'z') {
            // 해당 문자를 대문자로 바꿔줌
            str[i] -= 32;
        }
    }
    
    // 변환된 문자열을 반환
    return str;
}

// 테스트를 위한 입력 문자열
string input = "Hello, world!";

// capitalize() 함수를 호출하여 입력 문자열을 대문자로 변환
string output = capitalize(input);

// 변환 결과 출력
cout << output << endl;
```

위 코드를 실행하면, "HELLO, WORLD!"라는 대문자로 변환된 문자열이 출력됩니다.

## 깊이 파고들기:
(1) 과거의 문맥, (2) 대안들, (3) 문자열 대문자화의 구현 세부 사항 등, 문자열 대문자화에 대해 더 깊이 있는 정보를 제공합니다.

문자열 대문자화는 오래된 방법 중 하나로, 예전에는 컴퓨터에서 영어 이외의 언어들을 처리하는 데에 사용되었습니다. 하지만 현재에는 대부분의 언어에서 대문자/소문자를 구분하지 않는다는 점과, 영어를 사용하는 국가들이 많기 때문에 이 방법이 대중화되었습니다.

C++에서는 대문자로 변환하는 방법 외에도, string 클래스에 내장된 함수인 tolower()와 toupper() 함수를 사용하여 소문자로 변환하는 방법을 제공합니다.

이 외에도, 다른 언어에서도 대문자/소문자 변환에 대한 다양한 방법이 존재합니다. 예를 들어, 자바 스크립트에서는 toUpperCase() 함수를 사용합니다. 각 언어마다 다른 방법을 제공하니, 해당 언어의 공식 문서를 참고하는 것이 좋습니다.

## 관련 자료:
문자열 대문자화와 관련된 추가적인 정보를 얻을 수 있는 관련 자료들을 아래에 제공합니다.

- [C++ Reference: std::toupper](https://en.cppreference.com/w/cpp/string/byte/toupper)
- [C++ Reference: std::string::toupper](https://en.cppreference.com/w/cpp/string/basic_string/toupper)
- [JavaScript Reference: toUpperCase](https://www.w3schools.com/jsref/jsref_touppercase.asp)
- [Java API: String.toUpperCase()](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toUpperCase())