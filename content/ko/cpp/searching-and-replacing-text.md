---
title:                "텍스트 검색 및 교체"
html_title:           "C++: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트를 검색하고 바꾸는 것에 관심이 있는 독자라면 한글로 작성된 이 기사를 읽고 이해할 수 있습니다. 검색과 교체를 적절하게 이해하고 활용할 수 있게 도와드리겠습니다.

## 작업 방법

먼저, 필요한 C++ 라이브러리인 `string`과 `fstream`를 포함시켜야 합니다.
```C++
#include <iostream>
#include <string>
#include <fstream>
```

다음으로, 텍스트를 검색하고자 하는 파일을 `ifstream`를 이용해 열어줍니다.
```C++
ifstream inputFile("example.txt");
```

검색하고 싶은 단어나 문구를 사용자로부터 입력받은 후, 변수에 저장합니다.
```C++
string searchWord;
cout << "검색할 단어나 문구를 입력해주세요: ";
cin >> searchWord;
```

변수에 저장한 검색어를 파일에서 검색합니다. 여기서는 `find` 함수를 사용합니다. `find` 함수의 첫번째 매개변수는 검색하고자 하는 단어, 두번째 매개변수는 검색을 시작할 위치를 나타냅니다.
```C++
size_t position = word.find(searchWord, currentPosition);
```

검색한 단어가 발견되면 `replace` 함수를 이용해 원하는 단어나 문구로 교체합니다.
```C++
string replaceWord;
cout << "교체할 단어나 문구를 입력해주세요: ";
cin >> replaceWord;

word.replace(position, searchWord.length(), replaceWord);
```

이렇게 검색하고 교체한 내용을 파일에 출력하기 위해 `ofstream`를 이용해 파일을 다시 열어줍니다. `outFile` 변수를 이용해 파일에 쓸 수 있습니다.
```C++
ofstream outFile("output.txt");
if (outFile.is_open()) {
  outFile << word;
  outFile.close();
}
```

출력된 파일을 확인해보면 검색하고 교체가 잘 이루어졌을 것입니다.

## 깊게 파보기

C++에서는 `find` 함수와 `replace` 함수를 이용해 간단하게 검색과 교체를 할 수 있습니다. 그 외에도 `regex`라는 라이브러리를 사용하면 정규표현식을 이용해 더 복잡한 검색과 교체를 할 수 있습니다.

## 연관 기사

- [C++ Reference](https://en.cppreference.com/w/)
- [C++ Tutorial](https://www.tutorialspoint.com/cplusplus/index.htm)
- [C++ Standard Library](https://en.cppreference.com/w/cpp/header)