---
title:                "YAML로 작업하기"
aliases:
- /ko/lua/working-with-yaml/
date:                  2024-02-03T19:26:07.958032-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML로 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나요?

YAML은 "YAML Ain't Markup Language"의 약자로, 인간이 읽을 수 있는 데이터 직렬화 표준입니다. 일반적으로 설정 파일이나 언어 간 데이터 교환에 사용됩니다. 프로그래머들은 YAML의 단순성과 가독성 때문에 이를 선호하며, 이는 설정, 다양한 어플리케이션 구성 또는 비프로그래머가 편집해야 하는 내용에 적합합니다.

## 어떻게 사용하나요?

Lua는 기본적으로 YAML을 지원하지 않지만, `lyaml`과 같은 타사 라이브러리를 사용하여 YAML 파일을 작업할 수 있습니다. 이 라이브러리는 Lua와 YAML 데이터의 인코딩 및 디코딩을 가능하게 합니다. 먼저, Lua의 패키지 관리자인 LuaRocks를 통해 `lyaml`을 설치해야 합니다:

```bash
luarocks install lyaml
```

### YAML 디코딩:

예를 들어, `config.yaml`이라는 파일에 다음과 같은 YAML 내용이 있다고 가정해보겠습니다:

```yaml
database:
  host: localhost
  port: 3306
  username: user
  password: pass
```

다음 코드를 사용하여 이 YAML 파일을 Lua 테이블로 디코딩할 수 있습니다:

```lua
local yaml = require('lyaml')
local file = io.open("config.yaml", "r")
local content = file:read("*all")
file:close()

local data = yaml.load(content)
for k,v in pairs(data.database) do
  print(k .. ": " .. v)
end
```

이 스크립트를 실행하면 다음과 같은 출력이 나옵니다:

```output
host: localhost
port: 3306
username: user
password: pass
```

### YAML 인코딩:

Lua 테이블을 YAML 형식으로 인코딩하려면 `lyaml`에서 제공하는 `dump` 함수를 사용합니다. 다음과 같은 Lua 테이블의 YAML 표현을 생성하려는 경우를 생각해 보겠습니다:

```lua
local data = {
  website = {
    name = "Example",
    owner = "Jane Doe",
    metadata = {
      creation_date = "2023-01-01",
      tags = {"blog", "personal", "lua"}
    }
  }
}

local yaml = require('lyaml')
local yaml_data = yaml.dump({data})
print(yaml_data)
```

출력된 YAML은 다음과 같습니다:

```yaml
- website:
    metadata:
      creation_date: '2023-01-01'
      tags: [blog, personal, lua]
    name: Example
    owner: Jane Doe
```

이러한 패턴을 따르면, Lua 프로그래머는 다양한 어플리케이션을 위한 YAML 데이터를 효과적으로 관리할 수 있습니다. 이러한 YAML 작업은 다른 시스템 부분이나 직접적으로 다른 시스템과 원활하게 상호 작용하는 다양한 Lua 어플리케이션을 개발하는 데 중요합니다.
