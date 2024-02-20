---
date: 2024-01-20 17:38:39.307143-07:00
description: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\
  \uB294 \uAC83\uC740 \uB300\uC18C\uBB38\uC790\uB97C \uAD6C\uBD84\uD558\uC9C0 \uC54A\
  \uACE0 \uBE44\uAD50\uD558\uAC70\uB098 \uC815\uB82C\uD560 \uB54C \uC720\uC6A9\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130\uB97C\
  \ \uC77C\uAD00\uB418\uAC8C \uCC98\uB9AC\uD558\uACE0 \uC0AC\uC6A9\uC790 \uC785\uB825\
  \uC758 \uB300\uC18C\uBB38\uC790 \uC624\uB958\uB97C \uBC29\uC9C0\uD558\uAE30 \uC704\
  \uD574 \uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD569\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:14.748430
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uB294\
  \ \uAC83\uC740 \uB300\uC18C\uBB38\uC790\uB97C \uAD6C\uBD84\uD558\uC9C0 \uC54A\uACE0\
  \ \uBE44\uAD50\uD558\uAC70\uB098 \uC815\uB82C\uD560 \uB54C \uC720\uC6A9\uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130\uB97C \uC77C\
  \uAD00\uB418\uAC8C \uCC98\uB9AC\uD558\uACE0 \uC0AC\uC6A9\uC790 \uC785\uB825\uC758\
  \ \uB300\uC18C\uBB38\uC790 \uC624\uB958\uB97C \uBC29\uC9C0\uD558\uAE30 \uC704\uD574\
  \ \uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD569\uB2C8\uB2E4\
  ."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열을 소문자로 변환하는 것은 대소문자를 구분하지 않고 비교하거나 정렬할 때 유용합니다. 프로그래머들은 데이터를 일관되게 처리하고 사용자 입력의 대소문자 오류를 방지하기 위해 문자열을 소문자로 변환합니다.

## How to: (어떻게 하나요?)
Fish Shell에서 문자열을 소문자로 바꾸는 것은 간단합니다. `string lower` 명령을 사용하세요.

```fish
echo "Fish SHELL Is Fun!" | string lower
```

예상 출력:

```
fish shell is fun!
```

여러 단어를 포함한 문자열이든, 파일 이름이든, 코드 내 변수값이든 상관 없이 이 명령은 잘 동작합니다.

## Deep Dive (심층 붐뻡)
Fish Shell에서 문자열을 소문자로 바꾸는 기능은 유니코드 표준을 따르고 있어 모든 언어와 문자에 적용됩니다. `string lower`은 Fish 2.3.0부터 사용 가능하며 POSIX `tr` 명령어나 다른 셸의 내장 기능보다 사용하기 쉽습니다. 이 명령은 현재 로케일에 기반하여 동작하므로, 특정 언어에 특화된 문자 변환도 올바르게 처리할 수 있습니다. 예를 들어, 터키어에서는 'I'의 소문자가 'ı'가 됩니다.

```fish
set -lx LANG tr_TR.UTF-8
echo "I FISH" | string lower
```

예상 출력 (터키어 로케일에서):

```
ı fish
```

그러나 로케일에 따라 다른 결과가 나올 수 있으므로 주의해야 합니다.

## See Also (관련 자료)
- `string` 명령에 대한 공식 문서: [link](https://fishshell.com/docs/current/cmds/string.html)
- Fish Shell 문서와 튜토리얼: [link](https://fishshell.com/docs/current/index.html)
- 유니코드 문자 변환에 대한 자세한 정보: [Unicode Standard](http://www.unicode.org/standard/standard.html)
