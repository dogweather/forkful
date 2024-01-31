---
title:                "디버거 사용하기"
date:                  2024-01-26T03:47:42.952480-07:00
model:                 gpt-4-0125-preview
simple_title:         "디버거 사용하기"

category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/using-a-debugger.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
Bash에서 디버거를 사용한다는 것은 코드가 충돌하거나 은밀하게 행동을 잘못하게 만드는 버그와 같은 스크립트에서 문제를 테스트하고 찾기 위한 도구를 활용하는 것을 의미합니다. 프로그래머들이 이를 수행하는 이유는 라이브 환경에서 대혼란을 일으키기 전에 오류를 잡는 것이 훨씬 더 현명하기 때문입니다.

## 어떻게:
Bash는 다른 언어들처럼 내장형 디버거가 없지만, `set -x`와 같은 내장 명령어를 사용하여 무슨 일이 일어나고 있는지 추적할 수 있습니다. 또는 업그레이드를 위해서, 코드를 단계별로 진행할 수 있는 적절한 디버거인 `bashdb`가 있습니다. 여기에 예시가 있습니다:

```Bash
# set -x를 사용하여 디버깅하기
set -x
echo "디버깅 시작"
my_var="안녕, 디버깅 세계!"
echo $my_var
set +x

# bashdb 사용하기
# 패키지 관리자를 사용하여 bashdb 설치하기, 예: apt, yum, brew.
# my_script.sh라는 스크립트 디버깅하기:
bashdb my_script.sh
```

`set -x`를 사용하여 실행할 때의 출력:
```Bash
+ echo '디버깅 시작'
디버깅 시작
+ my_var='안녕, 디버깅 세계!'
+ echo '안녕, 디버깅 세계!'
안녕, 디버깅 세계!
+ set +x
```

## 심층 분석
역사적으로, Bash 스크립트를 디버깅하면 코드에 `echo` 문을 남발하는 것을 의미했습니다. 그러나 그 다음에 `set -x`가 등장하여 수동 출력 없이 런타임 실행을 엿볼 수 있게 되었습니다. 그리고 더 많은 제어를 원하는 사람들을 위해 C/C++용 gdb 디버거에서 영감을 받은 `bashdb` 디버거가 등장했습니다.

대안에 관해서는, `set` 명령어(`-x`, `-v`, `-e`)를 넘어, 다른 옵션으로는 분석을 위해 출력을 파일로 리다이렉션하거나 ShellCheck과 같은 외부 도구를 사용하여 정적 분석을 하는 것이 포함됩니다.

구현 측면에서, `set -x`는 쉽습니다; 실행되는 명령과 그 인자들을 출력하는 네이티브 Bash 옵션입니다. 반면에, `bashdb`는 코드를 단계별로 진행하고, 중단점을 설정하고, 표현식을 평가하는 것과 같은 기능을 허용합니다 - 이는 좀 더 까다로운 버그에 맞서 싸울 수 있는 기회를 제공합니다.

## 참고 자료
- Bash 디버거 프로젝트: http://bashdb.sourceforge.net/
- Chris Johnson과 Jayant Varma의 "Pro Bash Programming"을 통해 고급 스크립팅 학습.
- 정적 분석을 위한 ShellCheck: https://www.shellcheck.net/
