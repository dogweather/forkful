---
aliases:
- /ko/php/working-with-toml/
date: 2024-01-26 04:24:47.265754-07:00
description: "TOML\uC740 Tom's Obvious, Minimal Language\uC758 \uC904\uC784\uB9D0\uB85C\
  , JSON\uC774\uB098 YAML\uACFC \uC720\uC0AC\uD55C \uB370\uC774\uD130 \uD615\uC2DD\
  \uC774\uC9C0\uB9CC, \uC778\uAC04\uC774 \uC77D\uAE30 \uC27D\uB3C4\uB85D \uC124\uACC4\
  \uB418\uC5C8\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\
  \uC774\uD130 \uAD6C\uC870\uB85C \uC798 \uBCC0\uD658\uB418\uACE0 \uC9C1\uAD00\uC801\
  \uC774\uAE30 \uB54C\uBB38\uC5D0 \uC124\uC815 \uD30C\uC77C\uC6A9\uC73C\uB85C \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4."
lastmod: 2024-02-18 23:09:06.388696
model: gpt-4-0125-preview
summary: "TOML\uC740 Tom's Obvious, Minimal Language\uC758 \uC904\uC784\uB9D0\uB85C\
  , JSON\uC774\uB098 YAML\uACFC \uC720\uC0AC\uD55C \uB370\uC774\uD130 \uD615\uC2DD\
  \uC774\uC9C0\uB9CC, \uC778\uAC04\uC774 \uC77D\uAE30 \uC27D\uB3C4\uB85D \uC124\uACC4\
  \uB418\uC5C8\uC2B5\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\
  \uC774\uD130 \uAD6C\uC870\uB85C \uC798 \uBCC0\uD658\uB418\uACE0 \uC9C1\uAD00\uC801\
  \uC774\uAE30 \uB54C\uBB38\uC5D0 \uC124\uC815 \uD30C\uC77C\uC6A9\uC73C\uB85C \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
TOML은 Tom's Obvious, Minimal Language의 줄임말로, JSON이나 YAML과 유사한 데이터 형식이지만, 인간이 읽기 쉽도록 설계되었습니다. 프로그래머들은 데이터 구조로 잘 변환되고 직관적이기 때문에 설정 파일용으로 사용합니다.

## 방법:
먼저, `yosymfony/toml`과 같은 TOML 파서 라이브러리가 설치되어 있는지 확인하세요. TOML 파일을 파싱해 봅시다:

```php
composer require yosymfony/toml

<?php
require 'vendor/autoload.php';

use Yosymfony\Toml\Toml;

$tomlString = <<<TOML
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
TOML;

$array = Toml::Parse($tomlString);

print_r($array);
```

출력 예:

```
Array
(
    [database] => Array
        (
            [server] => 192.168.1.1
            [ports] => Array
                (
                    [0] => 8001
                    [1] => 8001
                    [2] => 8002
                )

            [connection_max] => 5000
            [enabled] => 1
        )

)
```
## 심층 분석
TOML은 2013년에 GitHub 공동 창립자인 Tom Preston-Werner에 의해 제작되었으며, XML과 JSON에 대한 사용자 친화적인 대안으로 구성 파일을 위해 고안되었습니다. JSON이 기계에게 단순하지만, TOML의 구조는 복잡한 YAML 없이 인간의 눈에 쉽습니다.

TOML의 대안으로는 JSON, YAML, XML 등이 있습니다. 각기 장점과 적용 시나리오가 있습니다. JSON은 언어 독립적이고 널리 사용되며; YAML은 더 읽기 쉽고 주석을 지원합니다; XML은 광범위하게 지원되고 확장성이 큽니다.

PHP에서 TOML을 구현할 때는, 그 내용을 PHP 배열이나 객체로 파싱하는 라이브러리를 봅니다. `yosymfony/toml`은 TOML v0.4.0 스펙을 준수하는 PHP 파서입니다. 최신 상태를 유지하기 위해서는 항상 더 새로운 파서나 가장 최신 TOML 버전(v1.0.0이 마지막 업데이트일 기준)을 지원하는 업데이트를 확인하세요.

## 참고
- TOML 사양: <https://toml.io/>
- PHP용 TOML 파서 (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- 데이터 형식 비교하기 (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- PHP 패키지 관리자 (Composer): <https://getcomposer.org/>
