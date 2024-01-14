---
title:                "C++: html 파싱"
simple_title:         "html 파싱"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## 왜 파싱 HTML을 하나요? 

파싱 HTML은 웹 개발에서 매우 유용합니다. 보통 웹 페이지는 HTML 형식으로 작성되는데, 이를 웹 애플리케이션에서 활용하기 위해서는 HTML 내용을 파싱하여 필요한 정보를 추출해야 합니다. 예를 들어, 웹 스크레이핑, 웹 크롤링 등을 할 때에도 HTML 파싱이 필요합니다. 

## 어떻게 파싱 HTML을 할 수 있나요? 

C++에서는 HTML 파싱을 하기 위해 다양한 라이브러리를 제공합니다. 여기서는 기본적으로 C++ 표준 라이브러리에 포함된 "regex" 라이브러리를 활용하는 방법을 소개하겠습니다. 아래 코드는 "test.html" 파일에서 "p" 태그에 포함된 내용을 파싱하여 출력하는 예제입니다. 

```C++
#include <iostream>
#include <fstream>
#include <regex>

using namespace std;

int main() {
    // HTML 파일을 읽어옵니다. 
    ifstream fin("test.html");
    string html((istreambuf_iterator<char>(fin)), istreambuf_iterator<char>());
    fin.close();

    // "p" 태그에 해당하는 문자열을 추출합니다. 
    regex pattern("<p.*?>(.*?)</p>");
    smatch matches;
    while (regex_search (html, matches, pattern)) {
        // 첫번째 그룹에 해당하는 부분만 출력합니다. 
        cout << matches[1] << endl;

        // 다음 매치를 위해 검색 범위를 업데이트 합니다. 
        html = matches.suffix().str();
    }
    return 0;
}
```

### 샘플 출력 
파싱 결과, "test.html" 파일에서는 두 개의 "p" 태그가 존재하며, 각각의 내용을 정상적으로 출력하고 있습니다. 

```
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam.
```

## 딥 다이브 
"regex" 라이브러리를 활용하여 간단한 HTML 파싱을 할 수 있다는 것을 알아보았습니다. 그러나 더 복잡한 HTML을 다룰 때에는 좀 더 고급 기술이 필요합니다. 예를 들어, HTML의 구조를 분석하여 트리 형태로 만들어주는 "libxml" 라이브러리 등을 사용할 수 있습니다. 또한, 웹 크롤러를 구현할 때는 웹 서버에서 HTML 파일을 다운로드 받아 파싱하는 방법보다는 웹 브라우저를 자동 조작하는 "Selenium" 라이브러리 등을 활용하는 것이 더 효율적일 수 있습니다. 

## 더 많은 정보 알아보기 
- [C++ regex 라이브러리 문서](https://en.cppreference.com/w/cpp/regex) 
- [libxml 라이브러리 공식 홈페이지](http://xmlsoft.org/) 
- [Selenium 라이브러리 공식 홈페이지](https://www.seleniumhq.org/) 

## 참고 자료 
- [Codecademy - Learn C++](https://www.codecademy.com/learn/learn-c-plus-plus) 
- [TutorialsPoint - C++ Regex](https://www.tutorialspoint.com/cpp_standard_library/cpp_regex.htm)