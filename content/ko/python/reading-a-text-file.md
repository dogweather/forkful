---
title:                "Python: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것은 프로그래밍에서 매우 중요합니다. 왜냐하면 텍스트 파일은 우리가 사용하는 모든 컴퓨터 프로그램의 기본 구성 요소이기 때문입니다. 따라서 텍스트 파일을 읽는 방법을 알아야만 프로그램을 더욱 확장하고 유용하게 만들 수 있습니다.

## 어떻게

텍스트 파일을 읽는 가장 간단한 방법은 'read()' 함수를 사용하는 것입니다. 이 함수는 해당 파일의 모든 내용을 하나의 문자열로 반환합니다. 예를 들어, 아래의 코드를 사용하여 "example.txt" 파일의 내용을 읽어올 수 있습니다.

```python
file = open("example.txt", "r")
content = file.read()
print(content)
```

출력된 결과는 "example.txt" 파일에 담겨있는 모든 내용을 보여줄 것입니다.

## 깊이 파고들기

텍스트 파일을 읽는 것은 실제로는 매우 복잡한 과정입니다. 파일을 읽으면서 파일 포인터가 한 줄씩 이동해야 하고, 파일의 끝인지 아닌지 확인해야 합니다. 또한 파일을 여러 번 읽을 수도 있어야 합니다.

따라서, 파일을 읽기 전에 우선 파일을 열어 주어야 합니다. 그리고 파일을 열었으면 읽기 모드('r')로 설정하고, 파일을 읽으면서 파일 포인터를 계속 이동해야 합니다. 마지막으로 파일을 닫아야 합니다.

또한 파일을 읽을 때 발생할 수 있는 오류에 대비해 예외 처리를 해주는 것이 좋습니다. 그리고 파일의 인코딩 방식도 지정해주어야 합니다. 모든 코드를 정확히 작성하고 실행해보면 텍스트 파일을 정확하게 읽을 수 있을 것입니다.

## 관련 자료

- [파이썬 입출력 방법](https://wikidocs.net/26)
- [파이썬 파일 입출력 관련 함수들](https://python.flowdas.com/library/io.html)
- [파이썬 예외 처리 방법](https://wikidocs.net/30)
- [파이썬 문자열 인코딩 방식](https://dojang.io/mod/page/view.php?id=2455)