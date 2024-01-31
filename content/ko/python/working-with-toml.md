---
title:                "프로그래머를 위한 TOML 다루기"
date:                  2024-01-26T04:25:41.481485-07:00
model:                 gpt-4-0125-preview
simple_title:         "프로그래머를 위한 TOML 다루기"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/working-with-toml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
TOML은 Tom's Obvious, Minimal Language의 약자로, JSON이나 YAML과 유사한 데이터 직렬화 형식이지만 간결함과 가독성을 목표로 합니다. 프로그래머들은 TOML을 구성 파일로 사용하는데, 이는 작성하기 쉽고 이해하기 쉬우며, 파이썬과 같은 프로그래밍 언어에서의 데이터 구조로 깔끔하게 매핑되기 때문입니다.

## 방법:
시작하기 전에, `pip install toml`을 사용하여 `toml` 패키지를 설치합시다. TOML 파일을 파싱해 봅시다:

```python
import toml

# 문자열로 된 예시 TOML 내용
toml_string = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # 첫 번째 등급 날짜

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# TOML 문자열 파싱
parsed_toml = toml.loads(toml_string)

# 데이터 접근
print(parsed_toml['owner']['name'])  # 출력: Tom Preston-Werner
print(parsed_toml['database']['ports'])  # 출력: [8001, 8001, 8002]
```

## 심층 탐구
TOML은 GitHub의 창립자 중 한 명인 Tom Preston-Werner에 의해 더 사용자 친화적인 구성 파일 형식으로 만들어졌습니다. 해시 테이블로 모호하지 않게 매핑되도록 설계되었고 기계가 쉽게 파싱할 수 있습니다.

JSON과 비교할 때, TOML은 구성 파일에서 더 읽기 쉽고 주석을 지원합니다. 또 다른 대안인 YAML은 더욱 간결할 수 있지만, 들여쓰기에 의존하고 탭이 허용되지 않는 것과 같은 미묘한 문제들로 인해 사람들을 혼란스럽게 할 수 있습니다.

실행 세부 사항과 관련하여, TOML 값은 문자열, 정수, 부동소수점, 불린, 날짜시간, 배열, 테이블을 포함한 타입이 있습니다. 모든 것은 대소문자를 구별합니다. 또한, TOML은 여러 줄 문자열을 지원하고, 최신 버전에서는 이질적으로 타입이 지정된 배열도 허용합니다.

파이썬은 `toml` 라이브러리를 사용하는데, API 측면에서 JSON 및 YAML 라이브러리와 유사합니다. 파일이나 문자열에서 TOML을 읽기 위해 `toml.load` 및 `toml.loads`를 사용하고, 이를 작성하기 위해 `toml.dump` 및 `toml.dumps`를 사용합니다.

## 참고
- 공식 TOML GitHub 저장소, 사양용: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `toml` 파이썬 라이브러리 문서: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- TOML의 실제 예: 러스트(Rust)의 패키지 관리자 `cargo`나 파이썬 패키징 도구 `poetry`의 구성 파일.
