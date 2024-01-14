---
title:                "Python: 컴퓨터 프로그래밍에서의 명령줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서의 명령줄 인수 읽기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 왜

우리는 대다수의 프로그램이 사용자로부터 입력을 받는 것을 본 적이 있습니다. 나중에 실행할 수도 있지만, 매번 다른 입력을 프로그램에 전달하는 것은 매우 번거로운 일입니다. 이때 명령 줄 인수를 사용하면 매번 입력을 다시 하지 않아도 되며, 프로그램을 더 효율적으로 사용할 수 있게 됩니다. 그렇기 때문에 명령 줄 인수를 배우는 것은 프로그래밍을 개발하는 데 매우 유용한 기술입니다.

## 어떻게

Python에서 명령 줄 인수를 읽는 것은 매우 간단합니다. 다음과 같은 코드를 사용하면 됩니다.

```Python
import sys

# 명령 줄 인수를 모두 가져오기
args = sys.argv

# 첫 번째 인수인 프로그램 이름 출력
print("프로그램 이름:", args[0])

# 두 번째 인수 이후는 모두 사용자가 입력한 값으로 출력
print("사용자 입력:", " ".join(args[1:]))
```

위 코드를 sample.py로 저장하고, 다음 명령어를 실행해보세요.

```
python sample.py Hello World!
```

다음과 같은 결과를 볼 수 있습니다.

```
프로그램 이름: sample.py
사용자 입력: Hello World!
```

이를 통해 sys.argv를 사용하여 명령 줄 인수를 읽어오고, 이를 활용하여 프로그램에 필요한 작업을 수행할 수 있음을 알 수 있습니다.

## 더 깊이 들어가기

명령 줄에서 인수를 읽는 것은 매우 유용하지만, 때로는 인수가 부족하거나 잘못된 형식인 경우 예외를 처리해야 할 수도 있습니다. 이때 sys.argv의 길이를 확인하거나 인수가 올바른 형식인지 확인하여 예외 처리를 수행할 수 있습니다.

또한, 인수를 더욱 복잡하게 처리하고 싶은 경우 argparse 모듈을 사용할 수도 있습니다. 이 모듈을 사용하면 명령 줄 인수를 손쉽게 분석하고 처리할 수 있습니다.

# See Also

- [Python 공식 문서 - sys.argv](https://docs.python.org/3/library/sys.html#sys.argv)
- [Python 공식 문서 - argparse](https://docs.python.org/3/library/argparse.html)
- [Real Python 블로그 - 명령 줄 인수 처리하기](https://realpython.com/command-line-interfaces-python-argparse/)