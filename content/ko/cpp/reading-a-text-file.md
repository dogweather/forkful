---
title:    "C++: 텍스트 파일 읽기"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

"## 왜: 텍스트 파일을 읽는 방법"

텍스트 파일을 읽는 것은 프로그래밍에 있어 매우 중요한 기술입니다. 텍스트 파일을 읽는 것은 다양한 정보를 읽고 처리하는 데 필요한 핵심 요소입니다. 따라서 프로그래밍을 배우거나 개발을 하고 있는 경우, 텍스트 파일을 읽는 방법을 익히는 것이 매우 중요합니다.

"## 사용 방법: 코드 블록 내에서 샘플 코드 및 출력 예제"

텍스트 파일을 읽는 것은 C++에서 매우 간단합니다. 우선, 파일을 읽기 위해 `<fstream>` 헤더를 불러와야 합니다. 그 다음, `ifstream` 객체를 생성하고 open() 함수를 사용하여 읽을 파일 이름을 전달합니다. 이제, `getline` 함수를 사용하여 파일에서 한 줄씩 데이터를 읽을 수 있습니다. 아래에 `text.txt` 파일 에서 10개의 숫자를 읽고 출력하는 예제 코드를 제공합니다.

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
  ifstream inputFile;
  inputFile.open("text.txt");

  int num;
  for (int i = 0; i < 10; i++) {
    inputFile >> num;
    cout << num << endl;
  }
  
  inputFile.close();

  return 0;
}
```

위의 코드를 실행하면, `text.txt` 파일에 있는 숫자들이 순서대로 출력됩니다. 즉, 파일을 끝까지 순서대로 읽는 것을 확인할 수 있습니다.

"## 심층 분석: 텍스트 파일을 읽는 더 깊은 정보"

위에서 언급한 예제는 매우 간단한 경우입니다. 하지만, 실제로는 파일의 크기가 매우 크거나, 특정 패턴을 찾는 등 더 복잡한 작업을 수행해야 할 때도 있습니다. 이를 위해 `feof()` 함수를 사용하면 파일의 끝에 도달했는지 여부를 확인할 수 있고, `seekg()` 함수를 사용하여 포인터를 파일에서 이동시킬 수도 있습니다. 또한, C++에서는 문자열로 이루어진 데이터도 쉽게 읽을 수 있습니다. 예를 들어, `getline` 함수를 사용하면 한 줄씩 데이터를 읽는 것이 아니라, 문자열로 이루어진 한 문장씩 읽을 수 있습니다.

"## 더 알아보기"

"## 더 자세한 내용 및 참고 자료"

- [C++으로 파일 다루기](https://dojang.io/mod/page/view.php?id=1809) : 경험 많은 프로그래머이신 이대환님의 블로그에서 C++으로 파일 다루기에 대해 자세히 설명하고 있습니다.
- [C++ 파일 입출력](https://modoocode.com/263) : 직관적인 예제와 함께 C++ 파일 입출력에 대한 설명을 제공하는 모두의 코드 블로그입니다.
- [C++ 파일 처리 예제](https://www.tips&tales.com/study/board/contents?board.id=cplusplus&page=2&mode=view&board.sid=33&gid=32&parent.id=202&dept=null) : 실제 프로그램을 만들면서 C++ 파일 처리를 적용하는 방법에 대해 상세한 설명을 해주는 블로그 포스트입니다.