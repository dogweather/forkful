---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:29.502397-07:00
description: "\uBC29\uBC95: Bash\uC5D0\uC11C\uB294 `>&2`\uB97C \uC0AC\uC6A9\uD558\uC5EC\
  \ \uCD9C\uB825\uC744 stderr\uB85C \uB9AC\uB2E4\uC774\uB809\uC158\uD569\uB2C8\uB2E4\
  . \uAE30\uBCF8 \uC608\uB294 \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.505743-06:00'
model: gpt-4-0125-preview
summary: "Bash\uC5D0\uC11C\uB294 `>&2`\uB97C \uC0AC\uC6A9\uD558\uC5EC \uCD9C\uB825\
  \uC744 stderr\uB85C \uB9AC\uB2E4\uC774\uB809\uC158\uD569\uB2C8\uB2E4."
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
weight: 25
---

## 방법:
Bash에서는 `>&2`를 사용하여 출력을 stderr로 리다이렉션합니다. 기본 예는 다음과 같습니다:

```bash
echo "This is a normal message"
echo "이것은 에러 메시지입니다" >&2
```

이 스크립트를 실행하면 두 메시지 모두 콘솔에 표시되지만, 리다이렉션하면 stdout과 stderr을 분리할 수 있습니다. 예를 들어:

```bash
bash script.sh > output.txt 2> error.txt
```

`output.txt`는 `"This is a normal message"`를 포함하고, `error.txt`는 `"이것은 에러 메시지입니다"`를 캡처할 것입니다.

실용적인 사용 사례를 고려해보면, 파일을 처리하고 파일이 존재하지 않으면 에러를 보고하는 스크립트입니다:

```bash
filename="example.txt"

if [ ! -f "$filename" ]; then
    echo "$filename does not exist!" >&2
    exit 1
else
    echo "Processing $filename"
fi
```

`example.txt`가 존재하지 않을 때 콘솔에서 직접적인 샘플 출력:

```
example.txt does not exist!
```

Bash에서 stderr를 처리하기 위한 직접적인 제3자 라이브러리는 없으며, 리다이렉션이 기본적으로 지원되어 일반적으로 충분합니다. 그러나 복잡한 애플리케이션의 경우, stdout과 stderr를 더 효과적으로 관리하기 위해 로깅 프레임워크나 외부 로깅 도구인 `syslog` 또는 `log4bash`와 같은 것을 도입할 수 있습니다.
