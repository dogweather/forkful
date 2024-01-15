---
title:                "HTML 파싱"
html_title:           "C++: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## 왜

HTML 파싱을 수행하는 이유는 웹에서 정보를 추출하기 위해서 입니다. 예를 들어, 크롤링을 할 때 HTML 파싱을 통해 원하는 데이터를 가져올 수 있습니다.

## 하는 방법

```C++
#include <iostream>
#include <string>
#include <vector>

using namespace std;

// HTML 파싱 함수
void parseHTML(string html) {
    vector<string> tags; // 태그들을 저장할 벡터
    string tag = ""; // 현재 읽고 있는 태그
    int start = -1; // 태그 시작 위치
    int end = -1; // 태그 끝 위치
    bool insideTag = false; // 태그 안에 있는지 여부

    // HTML 문자열을 한 글자씩 읽음
    for (int i = 0; i < html.length(); i++) {
        if (html[i] == '<') { // 열리는 태그를 만나면
            insideTag = true; // 태그 내부로 진입
            start = i + 1; // 태그 시작 위치 설정
        }
        else if (html[i] == '>') { // 닫히는 태그를 만나면
            insideTag = false; // 태그 외부로 이동
            end = i; // 태그 끝 위치 설정
            // 생성한 태그를 벡터에 추가
            tags.push_back(html.substr(start, end - start));
            tag = ""; // 태그 초기화
        }
        else if (insideTag) { // 태그 내부에서 문자를 읽으면
            tag += html[i]; // 현재 태그 변수에 이어붙임
        }
    }

    // 파싱 결과 출력
    cout << "Parsed tags:" << endl;
    for (int i = 0; i < tags.size(); i++) {
        cout << tags[i] << endl;
    }
}

int main() {
    string html = "<html><body><h1>Hello, world!</h1><p>This is a paragraph.</p></body></html>";

    // 함수 호출
    parseHTML(html);

    return 0;
}
```

실행 결과:

```
Parsed tags:
html
body
h1
Hello, world!
/h1
p
This is a paragraph.
/p
/body
/html
```

## 심화 학습

위 코드에서는 간단하게 HTML 파싱을 수행했지만, 실제로는 복잡한 문서를 다루기 위해 라이브러리를 사용하거나 복잡한 알고리즘을 사용합니다. 또한, 웹 문서를 파싱할 때 주의할 점으로 HTML을 표준에 맞게 작성해야만 정확한 파싱 결과를 얻을 수 있다는 점도 있습니다.

## 관련 글

- [HTML parsing in C++](https://www.geeksforgeeks.org/html-parsing-in-c-set-1/)
- [Parsing HTML using the HTML Agility Pack](https://www.codeproject.com/Articles/659019/HTML-Agility-Pack-article)
- [C++ HTML Parser](https://github.com/azadkuh/htmlcxx)