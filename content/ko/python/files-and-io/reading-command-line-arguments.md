---
date: 2024-01-20 17:56:50.289078-07:00
description: "(\uBB34\uC5C7\uACFC \uC65C?) \uCEE4\uB9E8\uB4DC \uB77C\uC778 \uC778\uC790\
  \ \uC77D\uAE30\uB294 \uD130\uBBF8\uB110\uC774\uB098 \uBA85\uB839 \uD504\uB86C\uD504\
  \uD2B8\uC5D0\uC11C \uD504\uB85C\uADF8\uB7A8\uC73C\uB85C \uB9E4\uAC1C\uBCC0\uC218\
  \uB97C \uC804\uB2EC\uD558\uB294 \uBC29\uBC95\uC778\uB370\uC694. \uD504\uB85C\uADF8\
  \uB7A8\uC744 \uC720\uC5F0\uD558\uAC8C \uB9CC\uB4E4\uACE0 \uC0AC\uC6A9\uC790\uAC00\
  \ \uC785\uB825\uC744 \uCEE4\uC2A4\uD140\uD560 \uC218 \uC788\uAC8C \uD574\uC8FC\uC8E0\
  ."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:13.562853
model: gpt-4-1106-preview
summary: "(\uBB34\uC5C7\uACFC \uC65C?) \uCEE4\uB9E8\uB4DC \uB77C\uC778 \uC778\uC790\
  \ \uC77D\uAE30\uB294 \uD130\uBBF8\uB110\uC774\uB098 \uBA85\uB839 \uD504\uB86C\uD504\
  \uD2B8\uC5D0\uC11C \uD504\uB85C\uADF8\uB7A8\uC73C\uB85C \uB9E4\uAC1C\uBCC0\uC218\
  \uB97C \uC804\uB2EC\uD558\uB294 \uBC29\uBC95\uC778\uB370\uC694. \uD504\uB85C\uADF8\
  \uB7A8\uC744 \uC720\uC5F0\uD558\uAC8C \uB9CC\uB4E4\uACE0 \uC0AC\uC6A9\uC790\uAC00\
  \ \uC785\uB825\uC744 \uCEE4\uC2A4\uD140\uD560 \uC218 \uC788\uAC8C \uD574\uC8FC\uC8E0\
  ."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
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
