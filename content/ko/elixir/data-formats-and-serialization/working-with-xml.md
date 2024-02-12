---
title:                "XML 다루기"
aliases: - /ko/elixir/working-with-xml.md
date:                  2024-01-26T04:30:04.375123-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML 다루기"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/working-with-xml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
엘릭서에서 XML을 다루는 것은 XML 데이터의 파싱, 생성, 조작을 의미합니다. 프로그래머들은 웹 서비스, 설정 파일, 그리고 레거시 시스템에서 널리 사용되기 때문에 XML을 다룹니다.

## 방법:
엘릭서는 표준 라이브러리에 XML 파싱 기능을 포함하고 있지 않습니다. SweetXML은 인기 있는 선택입니다. 사용 방법은 다음과 같습니다:

```elixir
# mix.exs에 SweetXML을 의존성으로 추가
{:sweet_xml, "~> 0.6"}

# 코드에서
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Don't forget me this weekend!</body>
</note>
"""

# XML 파싱
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # 출력: Tove
```

## 심층 탐구
XML, 또는 확장 가능 마크업 언어는 90년대 후반부터 사용되어왔습니다. 그것은 장황하지만 구조적이므로 복잡한 데이터 교환에 이상적입니다. JSON의 간결함으로 인해 인기가 급상승했음에도 불구하고, XML은 여전히 많은 기업 및 금융 시스템에서 그 표현력과 표준화된 스키마로 인해 깊이 자리 잡고 있습니다.

대안으로는 다음이 있습니다:
- 데이터 교환에 더 가벼우면서 덜 장황한 JSON.
- 내부 시스템, 특히 이진 직렬화된 데이터 통신에 대한 Protobuf 또는 Thrift.

내부적으로, 엘릭서의 XML 라이브러리는 파싱을 위해 엘랭의 :xmerl 라이브러리를 활용합니다. 이는 강력한 지원을 제공하지만 더 현대적인 접근법보다 직관성이 떨어질 수 있습니다. 엘릭서가 발전함에 따라, 커뮤니티 주도 라이브러리인 SweetXML과 같은 라이브러리는 이러한 기능을 더 엘릭서적인 문법으로 래핑함으로써 XML 조작을 더 접근하기 쉽게 만듭니다.

## 참고:
- SweetXML on Hex: https://hex.pm/packages/sweet_xml
- 엘릭서의 XML 파싱에 대한 접근: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- xmerl 문서로서 기본 XML 처리: http://erlang.org/doc/apps/xmerl/index.html
