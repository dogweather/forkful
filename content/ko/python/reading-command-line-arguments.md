---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 어떤 것 입니까 & 왜 그것이 필요한가요?
명령 줄 인자를 읽는 것은 사용자가 프로그램을 실행할 때 추가로 제공하는 매개변수를 읽는 것을 의미합니다. 이는 프로그래머에게 유연성을 제공하여 동일한 코드가 다양한 시나리오에서 작동하도록 할 수 있습니다.

## 사용방법:
```Python
# 필요한 모듈을 불러옵니다.
import sys

# 명령 줄 인자를 출력합니다.
print("이름: ", sys.argv[0])
print("인자: ", sys.argv[1:])
```
스크립트를 실행하면 다음과 같은 출력을 볼 수 있습니다 :
```Shell
$ python3 myscript.py arg1 arg2 arg3
이름:  myscript.py
인자:  ['arg1', 'arg2', 'arg3']
```

## Deep Dive
시간이 지남에 따라, `sys.argv` 와 같은 저수준의 기능 외에도 추가 기능을 제공하는 라이브러리도 등장하였습니다. 예를 들어, `argparse` 라이브러리는 강력한 인자 파싱 기능을 제공하며, `getopt` 라이브러리는 UNIX 스타일의 인자를 파싱하는 기능을 제공합니다.

대안으로, `click` 라이브러리는 간단한 스크립트에서 복잡한 CLI 어플리케이션까지 작성하기 위한 도구를 제공합니다. 이러한 라이브러리들은 사용자가 제공한 인자의 유효성을 검사하거나, 기본 값 및 에러 메시지를 설정하는 등의 편의성을 제공하곀으므로 `sys.argv`를 바로 사용하는 것보다 다소 복잡해 보일 수 있습니다. 

## 참조 자료
- Python 공식 문서에서 `sys.argv` 의 [문서](https://docs.python.org/3/library/sys.html#sys.argv)
- Python `argparse` 모듈 [문서](https://docs.python.org/3/library/argparse.html)
- 'click' 라이브러리 [소개 블로그](https://click.palletsprojects.com/en/7.x/)
- Python `getopt` 모듈 [문서](https://docs.python.org/3/library/getopt.html)