---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:27.142967-07:00
description: "Bash\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\uC2DD(regex)\uC744 \uC0AC\
  \uC6A9\uD558\uBA74 \uD2B9\uC815 \uD328\uD134\uC5D0 \uAE30\uBC18\uD558\uC5EC \uBB38\
  \uC790\uC5F4\uACFC \uD30C\uC77C\uC744 \uAC80\uC0C9, \uC870\uC791 \uBC0F \uCC98\uB9AC\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC785\uB825 \uAC80\uC99D, \uB85C\uADF8 \uD30C\uC77C \uD30C\uC2F1, \uB370\uC774\
  \uD130 \uCD94\uCD9C\uACFC \uAC19\uC740 \uC791\uC5C5\uC5D0 regex\uB97C \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4. \uC774\uB294 \uBCF5\uC7A1\uD55C \uD14D\uC2A4\uD2B8 \uCC98\uB9AC\
  \ \uC694\uAD6C\uC0AC\uD56D\uC5D0 \uB300\uD55C \uD328\uD134\uC744 \uC9C0\uC815\uD558\
  \uB294\u2026"
lastmod: '2024-03-13T22:44:55.464717-06:00'
model: gpt-4-0125-preview
summary: "Bash\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\uC2DD(regex)\uC744 \uC0AC\uC6A9\
  \uD558\uBA74 \uD2B9\uC815 \uD328\uD134\uC5D0 \uAE30\uBC18\uD558\uC5EC \uBB38\uC790\
  \uC5F4\uACFC \uD30C\uC77C\uC744 \uAC80\uC0C9, \uC870\uC791 \uBC0F \uCC98\uB9AC\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC785\
  \uB825 \uAC80\uC99D, \uB85C\uADF8 \uD30C\uC77C \uD30C\uC2F1, \uB370\uC774\uD130\
  \ \uCD94\uCD9C\uACFC \uAC19\uC740 \uC791\uC5C5\uC5D0 regex\uB97C \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4. \uC774\uB294 \uBCF5\uC7A1\uD55C \uD14D\uC2A4\uD2B8 \uCC98\uB9AC \uC694\
  \uAD6C\uC0AC\uD56D\uC5D0 \uB300\uD55C \uD328\uD134\uC744 \uC9C0\uC815\uD558\uB294\
  \u2026"
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
weight: 11
---

## 무엇 & 왜?

Bash에서 정규 표현식(regex)을 사용하면 특정 패턴에 기반하여 문자열과 파일을 검색, 조작 및 처리할 수 있습니다. 프로그래머들은 입력 검증, 로그 파일 파싱, 데이터 추출과 같은 작업에 regex를 사용합니다. 이는 복잡한 텍스트 처리 요구사항에 대한 패턴을 지정하는 유연하고 강력한 방법을 제공하기 때문입니다.

## 사용 방법:

### 기본 패턴 매칭
문자열이 패턴과 일치하는지 찾으려면 정규 표현식에 일치하는 행을 검색하는 명령어 라인 유틸리티인 `grep`을 사용할 수 있습니다:

```bash
echo "Hello, World!" | grep -o "World"
# 출력: World
```

### 특정 데이터 추출
regex 패턴과 일치하는 데이터 부분을 추출하려면 `grep`과 함께 `-o`를 사용할 수 있습니다:

```bash
echo "Error: File not found" | grep -oE "[A-Za-z]+:"
# 출력: Error:
```

### `sed`와 함께하는 Regex 사용
`sed` (스트림 편집기)는 텍스트를 파싱하고 변환하는 강력한 유틸리티입니다. 다음은 `sed`와 regex를 사용하여 텍스트를 대체하는 방법입니다:

```bash
echo "Bash is great" | sed -e 's/great/awesome/'
# 출력: Bash is awesome
```

### 조건문에서의 패턴 매칭
Bash는 조건문에서도 직접 regex를 지원합니다:

```bash
[[ "https://example.com" =~ ^https?:// ]] && echo "URL is valid" || echo "URL is invalid"
# 출력: URL is valid
```

### `awk`를 이용한 고급 패턴 매칭 및 조작
`awk`는 좀 더 복잡한 데이터 추출 및 조작을 지원하는 다른 텍스트 처리 도구입니다. CSV와 같은 구조화된 텍스트 데이터를 작업할 때 유용할 수 있습니다:

```bash
echo -e "ID,Name,Age\n1,John,22\n2,Jane,24" | awk -F, '$3 > 22 {print $2 " is older than 22."}'
# 출력: Jane is older than 22.
```

Bash 내장 regex 기능이 많은 사용 사례를 다루지만, 매우 고급 regex 작업의 경우, `perl`이나 `python` 스크립트와 Bash 스크립트의 조합을 고려할 수 있습니다. 이 언어들은 강력한 regex 라이브러리(e.g., Python의 `re`)를 제공합니다. Python으로 간단한 예시:

```bash
echo "Capture this 123" | python3 -c "import sys; import re; print(re.search('(\d+)', sys.stdin.read()).group(0))"
# 출력: 123
```

필요한 경우 이러한 프로그래밍 언어를 포함시키는 것은 Bash 스크립트에서 regex의 전체 힘을 활용할 수 있게 도와줄 수 있습니다.
