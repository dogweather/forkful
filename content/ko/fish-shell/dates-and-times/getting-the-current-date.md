---
title:                "현재 날짜 가져오기"
aliases:
- /ko/fish-shell/getting-the-current-date.md
date:                  2024-02-03T19:09:34.352563-07:00
model:                 gpt-4-0125-preview
simple_title:         "현재 날짜 가져오기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
