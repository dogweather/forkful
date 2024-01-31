---
title:                "JSON 다루기"
date:                  2024-01-19
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"

category:             "Elixir"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON은 JavaScript Object Notation의 약자로, 데이터 교환 포맷입니다. 프로그래머는 서로 다른 시스템 사이에서 데이터를 주고 받을 때 이 포맷을 사용합니다.

## How to:
Elixir에서 JSON을 다루려면 `Jason` 라이브러리를 사용하는 것이 일반적입니다.

```elixir
# 의존성에 Jason 추가
defp deps do
  [
    {:jason, "~> 1.2"}
  ]
end

# JSON 문자열을 Elixir 맵으로 파싱
json_string = "{\"key\": \"value\"}"
{:ok, result} = Jason.decode(json_string)
IO.inspect(result) # 출력: %{"key" => "value"}

# Elixir 맵을 JSON 문자열로 인코딩
map = %{key: "value"}
json_output = Jason.encode!(map)
IO.puts(json_output) # 출력: {"key":"value"}
```

## Deep Dive
JSON 처리에 관해 Elixir는 다양한 라이브러리를 가지고 있지만, `Jason`은 성능과 유연성 때문에 많은 Elixir 프로젝트에서 선호됩니다. 2001년에 JSON은 Douglas Crockford에 의해 고안되었으며, 웹 API 통신에 가장 널리 사용되는 포맷이 되었습니다. `Poison`이나 `jsx`는 `Jason`의 대안으로 사용될 수 있지만, 최근의 벤치마크에 따르면 `Jason`이 더 빠릅니다. Elixir에서는 라이브러리를 사용하여 JSON의 인코딩(맵을 JSON으로 변환)과 디코딩(JSON을 맵으로 변환)을 수행할 수 있습니다.

## See Also
- [Jason GitHub](https://github.com/michalmuskala/jason)
- [JSON 공식 사이트](https://www.json.org/json-ko.html)
