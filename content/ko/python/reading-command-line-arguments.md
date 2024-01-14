---
title:    "Python: 명령 줄 인수 읽기"
keywords: ["Python"]
---

{{< edit_this_page >}}

## 왜

Python으로 프로그래밍을 할 때, command line arguments를 읽는 것은 매우 중요합니다. command line arguments를 읽는 것으로 인해 사용자 입력을 프로그램에 전달할 수 있습니다. 이것은 프로그램을 사용할 때 매우 유용합니다.

## 사용 방법

Python에서는 `sys` 모듈을 사용하여 command line arguments를 읽을 수 있습니다. 아래의 예시 코드를 참고해보세요.

```Python
import sys

# 첫 번째 argument를 읽습니다.
first_arg = sys.argv[1]

# 두 번째 argument를 읽습니다.
second_arg = sys.argv[2]

print("첫 번째 argument: ", first_arg)
print("두 번째 argument: ", second_arg)
```

위의 예시 코드를 실행할 때, 아래와 같은 결과가 나올 것입니다.

```bash
$ python argument_reader.py argument1 argument2
첫 번째 argument: argument1
두 번째 argument: argument2
```

이와 같은 방식으로 여러 개의 argument를 읽을 수 있습니다. `sys.argv` 리스트에는 프로그램 이름과 첫 번째 argument가 포함되어 있습니다. 따라서 두 번째 argument부터 읽어야 합니다.

## 깊이 살펴보기

`sys.argv`는 프로그램에서 사용할 수 있는 여러 가지 정보를 포함하고 있습니다. 아래의 예시 코드를 통해 이를 확인해보세요.

```Python
import sys

# 모든 arguments를 읽습니다.
arguments = sys.argv

# arguments의 개수를 출력합니다.
print("Arguments의 개수: ", len(arguments))

# arguments의 내용을 모두 출력합니다.
print("Arguments: ", arguments)
```

위의 예시 코드를 실행할 때, 아래와 같은 결과가 나올 것입니다.

```bash
$ python argument_reader.py argument1 argument2
Arguments의 개수:  3
Arguments:  ['argument_reader.py', 'argument1', 'argument2']
```

간단하게 `sys.argv` 리스트를 통해 프로그램의 argument를 읽을 수 있습니다. 하지만 주의해야 할 점이 있습니다. 바로 사용자가 입력한 argument가 존재하지 않을 경우, `list index out of range`라는 에러가 발생한다는 것입니다. 이를 방지하기 위해 `if`문을 사용할 수 있습니다.

## 관련 자료

- [Python 공식 문서 - sys 모듈](https://docs.python.org/3/library/sys.html)
- [Real Python - Command Line Arguments in Python](https://realpython.com/python-command-line-arguments/)
- [GeeksforGeeks - Command Line Arguments in Python](https://www.geeksforgeeks.org/command-line-arguments-in-python/)