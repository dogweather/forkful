---
title:                "컴퓨터 프로그래밍의 기사 제목: 명령 줄 인수 읽기"
html_title:           "Python: 컴퓨터 프로그래밍의 기사 제목: 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍의 기사 제목: 명령 줄 인수 읽기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

* 왜 누군가는 커맨드 라인 인자를 읽어들이는 것에 관심을 가져야 할까요?
인공지능 분야나 데이터 분석을 하는 사람들은 대부분 파이썬을 사용하고 있으며, 파이썬에서는 인자를 읽어들이는 기능을 제공해줍니다. 따라서 커맨드 라인 인자를 읽는 방법을 알아두는 것은 파이썬을 더욱 효율적으로 사용하는 데 도움이 됩니다.

## 해결 방법

커맨드 라인 인자를 읽는 것은 매우 간단합니다. 우리는 `sys` 라이브러리를 이용하여 입력된 인자들을 리스트로 받아볼 수 있습니다. 아래 예제를 살펴봅시다.

```Python
import sys

# 저장된 파이썬 파일명도 인자로 포함되기 때문에 첫 번째는 무시합니다.
args = sys.argv[1:]

print("입력된 인자들: ", args)
```

위 코드를 실행하면, 우리가 파이썬 파일을 실행할 때 입력한 모든 인자들이 리스트 형태로 출력됩니다. 만약 `python arguments.py hello world`라는 명령어를 실행했다면, 아래와 같은 결과를 얻을 수 있습니다.

```
입력된 인자들: ['hello', 'world']
```

### 입력 인자들을 정수로 변환하기

`sys` 라이브러리를 이용하면 인자들을 문자열로만 받아오게 됩니다. 만약 정수로 변환하여 사용하고 싶다면 아래와 같이 코드를 작성할 수 있습니다.

```Python
import sys

args = sys.argv[1:]

# 정수로 변환되지 않는 인자는 제외함
numbers = [int(arg) for arg in args if arg.isdigit()]

print("정수로 변환된 인자들: ", numbers)
```

위 코드를 실행하면 문자열로 입력했던 정수 인자들이 정수형으로 변환되어 출력됩니다.

```
정수로 변환된 인자들: [1, 2, 3]
```

## 깊이 들어가기

파이썬에서 메인 프로그램을 실행할 때 커맨드 라인 인자들은 `sys.argv` 리스트에 저장됩니다. 이 리스트의 첫 번째 요소는 우리가 실행한 파이썬 파일의 경로를 포함하고 있기 때문에 무시해주어야 합니다. 따라서 실제 입력 인자들은 두 번째 요소부터 시작하게 됩니다. 또한 `sys.argv` 리스트는 인자들을 문자열로 받기 때문에 필요에 따라 형변환해주어야 합니다.

## See Also

* [Python `sys` 라이브러리 문서](https://docs.python.org/3/library/sys.html)
* [커맨드 라인 인자를 읽어들이는 다른 방법](https://stackoverflow.com/questions/2140191/python-how-to-read-different-type-of-command-line-arguments/2140235#2140235)