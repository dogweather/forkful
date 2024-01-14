---
title:    "Python: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인자를 읽는 것에 대해 배우는 이유는, 프로그램을 실행할 때 입력된 인자를 사용할 수 있기 때문입니다.

## 하는 방법

우선, 인자를 읽기 위해 sys 모듈을 임포트합니다.

```Python
import sys
```

다음으로, sys.argv를 사용하여 인자를 읽을 수 있습니다. 이는 리스트 형태로 저장되며, 첫 번째 인자는 실행 파일의 이름입니다.

```Python
arguments = sys.argv
```

원하는 인자를 사용하기 위해서는 인덱싱을 이용하면 됩니다.

```Python
first_argument = arguments[1] # 첫 번째 인자
second_argument = arguments[2] # 두 번째 인자
```

다음은 샘플 코드와 실행 결과입니다.

```Python
# sample.py

import sys

arguments = sys.argv

print("첫 번째 인자:", arguments[1])
print("두 번째 인자:", arguments[2])
```

```bash
$ python sample.py argument1 argument2
첫 번째 인자: argument1
두 번째 인자: argument2
```

## 깊이 파헤치기

커맨드 라인 인자를 사용하면, 사용자가 프로그램을 실행할 때 다양한 값을 입력할 수 있고, 이를 통해 더 유연한 프로그램을 만들 수 있습니다. 인자를 사용하면 입력 값을 프로그램 내에서 변수로 사용할 수 있으며, 이를 활용하여 다양한 기능을 추가할 수 있습니다.

## 참고 자료

[파이썬 공식 문서 - sys 모듈](https://docs.python.org/3/library/sys.html)

[Real Python - Command-Line Arguments in Python](https://realpython.com/python-command-line-arguments/)

[Python Tips - Command Line Arguments in Python](https://pythontips.com/2013/08/04/args-and-kwargs-in-python-explained/)

See Also

[커맨드 라인 인자 사용하기](https://wikidocs.net/93917)