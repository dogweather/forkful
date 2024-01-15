---
title:                "텍스트 파일 읽기"
html_title:           "C++: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것이 왜 중요한지 알고 싶다면 이 글을 읽어보세요.

## 방법

```C++
#include <iostream>
#include <fstream>

int main() {
  // 텍스트 파일을 가져오고 변수에 저장
  std::ifstream file("textfile.txt");
  std::string text;

  // 파일에서 한 줄씩 읽어와서 출력
  while (getline(file, text)) {
    std::cout << text << std::endl;
  }

  // 파일 닫기
  file.close();

  return 0;
}
```

**출력:**

```
오늘의 할 일:
- 운동하기
- 책 읽기
- 친구들 만나기
```

## 깊이 파고들기

텍스트 파일을 읽는 것은 데이터 분석, 로그 파일 분석, 문서 처리 등 다양한 분야에서 필요한 기능입니다. C++에서는 `ifstream` 라이브러리를 사용하여 쉽게 파일을 열고 읽을 수 있습니다. 또한 `while` 루프를 사용하여 파일의 끝까지 계속해서 읽을 수 있습니다.

## 더 보기

- [C++ ifstream 문서](https://www.cplusplus.com/reference/fstream/ifstream/)
- [C++ 파일 처리 예제](https://www.programiz.com/cpp-programming/file-handling)
- [C++ 파일 입출력 강좌](https://dojang.io/mod/page/view.php?id=518)