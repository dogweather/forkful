---
date: 2024-01-26 04:21:43.926203-07:00
description: "\uBC29\uBC95: Fish\uC5D0\uC11C TOML\uC744 \uC77D\uACE0 \uC870\uC791\uD558\
  \uB824\uBA74 TOML\uC744 JSON\uC73C\uB85C \uBCC0\uD658\uD560 \uC218 \uC788\uB294\
  \ `yj`\uC640 \uAC19\uC740 \uB3C4\uAD6C\uB97C \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4. \uBC29\uBC95\uC740 \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.889682-06:00'
model: gpt-4-0125-preview
summary: "Fish\uC5D0\uC11C TOML\uC744 \uC77D\uACE0 \uC870\uC791\uD558\uB824\uBA74\
  \ TOML\uC744 JSON\uC73C\uB85C \uBCC0\uD658\uD560 \uC218 \uC788\uB294 `yj`\uC640\
  \ \uAC19\uC740 \uB3C4\uAD6C\uB97C \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  ."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
weight: 39
---

## 방법:
Fish에서 TOML을 읽고 조작하려면 TOML을 JSON으로 변환할 수 있는 `yj`와 같은 도구를 사용할 수 있습니다. 방법은 다음과 같습니다:

```fish
# Fisher를 통해 yj 설치
fisher install jorgebucaran/yj

# TOML을 JSON으로 변환
echo 'title = "TOML 예제"' | yj -tj

# 샘플 출력
{"title":"TOML 예제"}
```

TOML을 작성하려면, 과정을 반대로 실행합니다:

```fish
# JSON을 TOML로 변환
echo '{"title":"JSON 예제"}' | yj -jt

# 샘플 출력
title = "JSON 예제"
```

많은 양의 작업을 처리해야 한다면, `toml-cli`와 같은 전용 TOML CLI 도구를 고려해보세요.

```fish
# toml-cli 설치
pip install toml-cli

# TOML 파일에 값 설정
toml set pyproject.toml tool.poetry.version "1.1.4"

# TOML 파일에서 값 가져오기
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## 심층 분석
TOML (Tom's Obvious, Minimal Language)은 2013년에 Tom Preston-Werner에 의해 소개되었으며, 정의된 사양과 데이터 계층 구조를 가진 INI와 유사합니다. 주요 대안으로는 JSON과 YAML이 있지만, 각각의 단점이 있습니다: JSON은 인간 친화적이지 않으며, YAML은 더 복잡합니다. TOML의 디자인은 설정 파일을 자주 수동으로 관리해야 하는 시나리오에서 번성하며, 단순성과 표현력 사이의 균형을 맞춥니다. 구현과 관련하여, 대부분의 프로그래밍 언어에 대한 TOML 파서가 사용 가능하며, 스크립트에 바로 통합될 수 있는 Fish 용 TomlBombadil도 포함됩니다.

## 참고자료
- TOML 공식 사양: https://toml.io
- `yj`, TOML, JSON, YAML, XML 간 변환을 위한 도구: https://github.com/jorgebucaran/yj
- `toml-cli`, TOML을 위한 커맨드라인 유틸리티: https://github.com/sdispater/toml-cli
