---
title:                "명령줄 인수 읽기"
aliases:
- /ko/python/reading-command-line-arguments/
date:                  2024-01-20T17:56:50.289078-07:00
model:                 gpt-4-1106-preview
simple_title:         "명령줄 인수 읽기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
(무엇과 왜?)
커맨드 라인 인자 읽기는 터미널이나 명령 프롬프트에서 프로그램으로 매개변수를 전달하는 방법인데요. 프로그램을 유연하게 만들고 사용자가 입력을 커스텀할 수 있게 해주죠.

## How to:
(어떻게 하나요?)
파이썬에서는 `sys` 모듈의 `argv`를 사용하여 커맨드 라인 인자를 읽습니다. 아래 예시 코드와 결과를 참고하세요.

```Python
import sys

# 인자를 출력합니다.
print("Script Name:", sys.argv[0])
for i, arg in enumerate(sys.argv[1:]):
    print(f"Argument {i+1}: {arg}")

# 사용해 보세요. 터미널에 이렇게 입력해 볼까요?
# python script.py first_arg second_arg third_arg
```

예상 출력:
```
Script Name: script.py
Argument 1: first_arg
Argument 2: second_arg
Argument 3: third_arg
```

## Deep Dive
(깊이 파기)
커맨드 라인 인자 읽기는 유닉스 시대부터 있었습니다. 이 방식은 단순히 스크립트를 실행하는 것보다 유연성을 더해준답니다. `argparse`나 `click` 같은 라이브러리로 더 발전시킬 수도 있어요. 이 라이브러리들은 인자를 더 쉽게 파싱하게 해주고, 사용자에게 명확한 도움말을 제공해 줍니다.

## See Also
(참고자료)
- [Python `sys` module documentation](https://docs.python.org/3/library/sys.html)
- [The `argparse` module for more complex argument parsing](https://docs.python.org/3/library/argparse.html)
- [The `click` library for creating beautiful command line interfaces](https://click.palletsprojects.com/en/7.x/)
