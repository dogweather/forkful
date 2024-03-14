---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:34.352563-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\
  \uB97C \uC5BB\uB294 \uAC83\uC740 \uC2DC\uC2A4\uD15C\uC758 \uB0A0\uC9DC \uBC0F \uC2DC\
  \uAC04 \uB370\uC774\uD130\uB97C \uAC80\uC0C9\uD558\uACE0 \uC870\uC791\uD560 \uC218\
  \ \uC788\uAC8C \uD574\uC8FC\uB294 \uAE30\uBCF8\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\
  \uB2E4. \uC2A4\uD06C\uB9BD\uD305 \uBC0F \uC790\uB3D9\uD654 \uC791\uC5C5\uC5D0\uC11C\
  \ \uD0C0\uC784\uC2A4\uD0EC\uD504 \uC0DD\uC131, \uC791\uC5C5 \uC608\uC57D, \uB85C\
  \uADF8 \uC0DD\uC131 \uB4F1\uC5D0 \uD544\uC218\uC801\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.871398-06:00'
model: gpt-4-0125-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C\
  \ \uC5BB\uB294 \uAC83\uC740 \uC2DC\uC2A4\uD15C\uC758 \uB0A0\uC9DC \uBC0F \uC2DC\uAC04\
  \ \uB370\uC774\uD130\uB97C \uAC80\uC0C9\uD558\uACE0 \uC870\uC791\uD560 \uC218 \uC788\
  \uAC8C \uD574\uC8FC\uB294 \uAE30\uBCF8\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4\
  . \uC2A4\uD06C\uB9BD\uD305 \uBC0F \uC790\uB3D9\uD654 \uC791\uC5C5\uC5D0\uC11C \uD0C0\
  \uC784\uC2A4\uD0EC\uD504 \uC0DD\uC131, \uC791\uC5C5 \uC608\uC57D, \uB85C\uADF8 \uC0DD\
  \uC131 \uB4F1\uC5D0 \uD544\uC218\uC801\uC785\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
프로그래밍에서 현재 날짜를 얻는 것은 시스템의 날짜 및 시간 데이터를 검색하고 조작할 수 있게 해주는 기본적인 작업입니다. 스크립팅 및 자동화 작업에서 타임스탬프 생성, 작업 예약, 로그 생성 등에 필수적입니다.

## 사용 방법:
Fish Shell은 `date`와 같은 외부 명령어를 사용하여 현재 날짜를 얻으며, 필요에 따라 출력 형식을 자유롭게 지정할 수 있게 해줍니다. 사용 방법은 다음과 같습니다:

```fish
# 기본 형식으로 현재 날짜 표시
echo (date)

# 출력 예시: Wed 25 Oct 2023 15:42:03 BST
```

날짜의 형식을 사용자 지정하려면 형식 지정자를 뒤따르는 `+` 옵션을 사용할 수 있습니다:

```fish
# YYYY-MM-DD 형식으로 현재 날짜 표시
echo (date "+%Y-%m-%d")

# 출력 예시: 2023-10-25
```

타임스탬프로 작업하거나 날짜 산술을 수행하는 등 더 복잡한 작업을 위해 Fish Shell은 스크립팅하는 성격 때문에 `date`와 같은 외부 도구에 의존합니다. 여기 현재 UNIX 타임스탬프를 얻는 예가 있습니다:

```fish
# 현재 UNIX 타임스탬프 얻기
echo (date "+%s")

# 출력 예시: 1666710123
```

`date`를 사용하여 현재 날짜에 하루를 더하는 방법:

```fish
# 현재 날짜에 하루를 더하기
echo (date -d "+1 day" "+%Y-%m-%d")

# 출력 예시: 2023-10-26
```

주의: 예제들은 GNU coreutils와 함께 동작하는 `date` 명령어 옵션을 사용합니다. macOS와 같은 다른 환경에서는 기본적으로 BSD date 명령어를 사용하기 때문에 옵션이 다를 수 있습니다. 환경에 특정된 세부 사항은 항상 `date --help` 또는 man 페이지를 참조하세요.
