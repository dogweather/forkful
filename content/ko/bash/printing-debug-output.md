---
title:                "디버그 출력을 인쇄하기"
html_title:           "Clojure: 디버그 출력을 인쇄하기"
simple_title:         "디버그 출력을 인쇄하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

디버그 출력이란 프로그램이 내부에서 무슨 일이 일어나고 있는지 확인하기 위해 사용하는 정보입니다. 이것은 개발자들이 프로그램의 문제를 찾고, 해결하는데 도움이 됩니다.

## 어떻게 하나요?

다음은 디버그 출력을 하는 Bash 스크립트의 예시입니다.

```Bash
#!/bin/bash

for i in {1..5}
do
   echo "디버깅: 현재 숫자는 $i 입니다." >&2
done
```
스크립트를 실행하면, 다음을 출력합니다:
```Bash
디버깅: 현재 숫자는 1 입니다.
디버깅: 현재 숫자는 2 입니다.
디버깅: 현재 숫자는 3 입니다.
디버깅: 현재 숫자는 4 입니다.
디버깅: 현재 숫자는 5 입니다.
```
## 깊게 알아보기

디버그 출력은 프로그래밍 역사와 함께 발전해왔습니다. 원시적인 방법으로는 코드에 '프린트' 문장을 포함시켜 콘솔로 출력하는 것이었습니다. 하지만 현재는 보다 발전된 도구들이 존재합니다. 

Bash에서는 `set -x` 및 `set +x` 명령을 사용하여 디버그 출력을 쉽게 해제, 설정할 수 있습니다.

```Bash
#!/bin/bash

set -x
for i in {1..5}
do
   echo "디버깅: 현재 숫자는 $i 입니다."
done
set +x
```
출력에 '+' 기호가 있는 이유는 수정하기 편하도록 각 명령 앞에 추가한 것입니다.

대안으로 'logger' 명령을 사용하여 시스템 로그에 메시지를 남길 수도 있습니다. 이 방법은 원격 시스템에서 작업할 때 특히 유용합니다.

## 참고자료

디버그 출력의 개념 및 다른 디버깅 기법을 배우려면 다음 웹사이트를 참조하세요:

- [GNU Bash 수동](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash 디버깅 기법](https://www.linuxjournal.com/content/debugging-bash-scripts)
- [Bash Script의 고급 디버깅](http://bashdb.sourceforge.net/remake/advanced-bash-debugging.html)