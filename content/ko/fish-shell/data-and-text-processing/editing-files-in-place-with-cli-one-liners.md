---
date: 2024-01-27 16:21:26.234338-07:00
description: "\uBA85\uB839\uC904 \uC778\uD130\uD398\uC774\uC2A4(CLI) \uC6D0-\uB77C\
  \uC774\uB108\uB97C \uC0AC\uC6A9\uD55C \uD30C\uC77C\uC758 \uD604\uC7A5 \uC218\uC815\
  \uC740 \uD14D\uC2A4\uD2B8 \uD3B8\uC9D1\uAE30\uC5D0\uC11C \uD30C\uC77C\uC744 \uC5F4\
  \uC9C0 \uC54A\uACE0 \uBA85\uB839\uC904\uC5D0\uC11C \uC9C1\uC811 \uD30C\uC77C\uC5D0\
  \ \uBCC0\uACBD\uC744 \uAC00\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB7EC\uD55C \uBC29\uBC95\uC744 \uC0AC\
  \uC6A9\uD558\uC5EC \uC2DC\uAC04\uC744 \uC808\uC57D\uD558\uACE0 \uBC18\uBCF5\uC801\
  \uC778 \uD3B8\uC9D1 \uC791\uC5C5\uC744 \uC790\uB3D9\uD654\uD568\uC73C\uB85C\uC368\
  , \uADF8\uB4E4\uC758 \uC791\uC5C5 \uD750\uB984\uC744 \uB354\uC6B1\u2026"
lastmod: 2024-02-19 22:05:14.764205
model: gpt-4-0125-preview
summary: "\uBA85\uB839\uC904 \uC778\uD130\uD398\uC774\uC2A4(CLI) \uC6D0-\uB77C\uC774\
  \uB108\uB97C \uC0AC\uC6A9\uD55C \uD30C\uC77C\uC758 \uD604\uC7A5 \uC218\uC815\uC740\
  \ \uD14D\uC2A4\uD2B8 \uD3B8\uC9D1\uAE30\uC5D0\uC11C \uD30C\uC77C\uC744 \uC5F4\uC9C0\
  \ \uC54A\uACE0 \uBA85\uB839\uC904\uC5D0\uC11C \uC9C1\uC811 \uD30C\uC77C\uC5D0 \uBCC0\
  \uACBD\uC744 \uAC00\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB7EC\uD55C \uBC29\uBC95\uC744 \uC0AC\uC6A9\
  \uD558\uC5EC \uC2DC\uAC04\uC744 \uC808\uC57D\uD558\uACE0 \uBC18\uBCF5\uC801\uC778\
  \ \uD3B8\uC9D1 \uC791\uC5C5\uC744 \uC790\uB3D9\uD654\uD568\uC73C\uB85C\uC368, \uADF8\
  \uB4E4\uC758 \uC791\uC5C5 \uD750\uB984\uC744 \uB354\uC6B1\u2026"
title: "CLI \uD55C \uC904 \uBA85\uB839\uC5B4\uB85C \uD30C\uC77C\uC744 \uC81C\uC790\
  \uB9AC\uC5D0\uC11C \uD3B8\uC9D1\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

명령줄 인터페이스(CLI) 원-라이너를 사용한 파일의 현장 수정은 텍스트 편집기에서 파일을 열지 않고 명령줄에서 직접 파일에 변경을 가하는 것을 말합니다. 프로그래머들은 이러한 방법을 사용하여 시간을 절약하고 반복적인 편집 작업을 자동화함으로써, 그들의 작업 흐름을 더욱 원활하고 효율적으로 만듭니다.

## 방법:

사용자 친화적 기능과 강력한 스크립팅 능력으로 알려진 Fish Shell은 현장에서 파일을 수정하는 여러 방법을 제공합니다. 하지만, 다른 쉘들과는 달리, Fish는 현장 수정을 위한 내장 메커니즘이 없습니다(`sed -i` in Bash, 예를 들면). 하지만 걱정하지 마세요, `sed`와 `awk` 같은 외부 도구의 도움으로 조금의 창의력을 사용하여 이를 달성할 수 있습니다.

### 간단한 교체를 위한 `sed` 사용
`file.txt`에서 "hello"의 모든 인스턴스를 "world"로 교체하려면, 다음을 사용하십시오:
```Fish Shell
sed -i '' 's/hello/world/g' file.txt
```

### 여러 `sed` 명령 적용
여러 교체를 수행해야 하는 경우, 이를 다음과 같이 연결할 수 있습니다:
```Fish Shell
sed -i '' -e 's/fish/bass/g' -e 's/rainbow/trout/g' file.txt
```

### 더 복잡한 연산을 위한 `awk` 사용
`sed`로 해결하기에 너무 복잡한 연산이라면, `awk`가 당신의 도구가 될 수 있습니다. 각 라인의 숫자를 두 배로 늘리는 방법은 다음과 같습니다:
```Fish Shell
awk '{print $1 * 2}' file.txt > temp && mv temp file.txt
```

### 오류 처리에 대한 주의사항
Fish에서 이러한 도구를 사용할 때, 오류를 감지하고 그 메시지를 이해하는 것이 중요합니다. Fish의 강력한 오류 처리를 사용하여 스크립트를 더욱 신뢰할 수 있게 만드세요.

## 깊이 있게 알아보기

역사적으로, 파일의 현장 수정은 Unix 및 Linux 프로그래밍의 기본이었으며, 파일을 수동으로 열지 않고 빠른 편집을 수행하는 효율적인 방법을 제공합니다. `sed`와 `awk`와 같은 도구는 Unix 초기부터 사용되어 왔으며, 텍스트 처리 작업에 없어서는 안 될 필수 유틸리티가 되었습니다.

Fish Shell은 사용성과 스크립팅에서 개선을 자랑하면서도, 대화형 및 사용자 친화적인 디자인 철학에 중점을 둔 결과로 내장 현장 수정 기능이 없습니다. Fish에서 네이티브 현장 수정 명령이 없다는 사실은 Unix 계열 생태계에서 외부 도구의 중요성을 강조합니다.

Fish에서 현장 수정을 위한 대안에는 임시 파일을 사용하거나 Perl 또는 Python 원-라이너를 활용하는 방법이 포함됩니다. 이는 복잡한 작업에 대해 더 많은 유연성이나 가독성을 제공할 수 있습니다.

예를 들어, Perl 사용:
```Fish Shell
perl -pi -e 's/find/replace/g' file.txt
```
또는 Python:
```Fish Shell
python -c "import re, sys; [sys.stdout.write(re.sub('pattern', 'replacement', line)) for line in sys.stdin]" < file.txt > temp && mv temp file.txt
```

구현 측면에서 볼 때, 현장 수정을 수행할 때, 이러한 도구들은 일반적으로 임시 파일을 생성하여 변경 사항을 그곳에 작성한 다음, 수정된 버전으로 원본 파일을 교체합니다. 이 방법은 작업 중 오류가 발생하여 데이터가 손상되거나 손실되지 않도록 파일 편집 과정을 보장합니다.

이러한 도구와 방법을 이해함으로써, Fish Shell 프로그래머는 현장 수정을 그들의 스크립트에 효과적으로 통합할 수 있으며, Fish의 사용자 친화적 기능과 전통적인 Unix 텍스트 처리 유틸리티의 원시력 사이의 간극을 메울 수 있습니다.
