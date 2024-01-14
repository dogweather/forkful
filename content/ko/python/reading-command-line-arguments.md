---
title:                "Python: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

명령 줄 인수를 읽는 방법을 배우는 것은 파이썬 프로그래밍에 있어 중요합니다. 명령 줄 인수를 사용하면 사용자가 프로그램을 실행할 때 프로그램에 추가적인 정보를 전달할 수 있습니다.

## 사용 방법

먼저, `sys` 모듈을 임포트하고 `argv` 속성을 사용하여 명령 줄 인수를 읽습니다. 매개변수 `argv`는 문자열의 리스트로 이루어져 있으며, 각 문자열은 사용자가 프로그램 실행 시에 추가한 인수입니다.

```Python
import sys

# 첫 번째 인수는 항상 파일 이름이므로 무시합니다.
arguments = sys.argv[1:]

# 문자열로 이루어진 리스트로 출력됩니다.
print(arguments)
```

만약 사용자가 다음과 같은 명령을 실행할 경우:

```bash
python my_program.py arg1 arg2 arg3
```

`['arg1', 'arg2', 'arg3']`가 출력됩니다.

출력된 인수를 다양한 방법으로 활용할 수 있습니다. 예를 들어, `argparse` 모듈을 사용하여 지정한 이름의 플래그를 사용하고 그에 대한 값을 저장할 수 있습니다. 또는 각 인수에 대해 `if`문을 사용하여 원하는 작업을 수행할 수도 있습니다.

## 깊게 들어가기

명령 줄 인수를 읽는 방법은 매우 유용하지만, 주의할 점도 있습니다. 첫 번째로, 사용자가 정확한 인자의 개수를 제공하지 않을 수 있기 때문에 예외 처리를 꼭 해주어야 합니다. 또한 인수들 사이의 공백, 따옴표 등이 서로 다른 운영체제에서 다르게 처리될 수 있기 때문에 이에 대한 처리도 필요합니다.

명령 줄 인수를 사용하여 보다 유연하고 다양한 입력을 받을 수 있는 프로그램을 작성할 수 있으며, 파이썬의 강력한 기능 중 하나로 꼭 익혀두어야 합니다.

## 참고자료

- [Python docs - sys module](https://docs.python.org/3/library/sys.html)
- [RealPython - Command-line arguments in Python](https://realpython.com/python-command-line-arguments/)
- [GeeksforGeeks - Handling command line arguments in Python](https://www.geeksforgeeks.org/handling-command-line-arguments-in-python/)