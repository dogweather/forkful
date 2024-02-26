---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:22.035671-07:00
description: "Haskell\uC5D0\uC11C HTML\uC744 \uD30C\uC2F1\uD558\uB294 \uAC83\uC740\
  \ \uB370\uC774\uD130\uB97C \uCD94\uCD9C\uD558\uACE0, HTML \uB0B4\uC6A9\uC744 \uC870\
  \uC791\uD558\uBA70, \uD504\uB85C\uADF8\uB798\uBC0D\uC801\uC73C\uB85C \uC6F9 \uD398\
  \uC774\uC9C0\uC640 \uC0C1\uD638\uC791\uC6A9\uD558\uB294 \uB370\uC5D0 \uC0AC\uC6A9\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uC6F9 \uC2A4\uD06C\
  \uB798\uD551, \uC6F9 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC758 \uC790\uB3D9\uD654\
  \uB41C \uD14C\uC2A4\uD2B8, \uC6F9\uC0AC\uC774\uD2B8\uC5D0\uC11C\uC758 \uB370\uC774\
  \uD130 \uB9C8\uC774\uB2DD \uB4F1\uC758 \uC791\uC5C5\uC5D0 \uD544\uC218\uC801\uC774\
  \uBA70,\u2026"
lastmod: '2024-02-25T18:49:52.287252-07:00'
model: gpt-4-0125-preview
summary: "Haskell\uC5D0\uC11C HTML\uC744 \uD30C\uC2F1\uD558\uB294 \uAC83\uC740 \uB370\
  \uC774\uD130\uB97C \uCD94\uCD9C\uD558\uACE0, HTML \uB0B4\uC6A9\uC744 \uC870\uC791\
  \uD558\uBA70, \uD504\uB85C\uADF8\uB798\uBC0D\uC801\uC73C\uB85C \uC6F9 \uD398\uC774\
  \uC9C0\uC640 \uC0C1\uD638\uC791\uC6A9\uD558\uB294 \uB370\uC5D0 \uC0AC\uC6A9\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uC6F9 \uC2A4\uD06C\
  \uB798\uD551, \uC6F9 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC758 \uC790\uB3D9\uD654\
  \uB41C \uD14C\uC2A4\uD2B8, \uC6F9\uC0AC\uC774\uD2B8\uC5D0\uC11C\uC758 \uB370\uC774\
  \uD130 \uB9C8\uC774\uB2DD \uB4F1\uC758 \uC791\uC5C5\uC5D0 \uD544\uC218\uC801\uC774\
  \uBA70,\u2026"
title: "HTML \uD30C\uC2F1"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Haskell에서 HTML을 파싱하는 것은 데이터를 추출하고, HTML 내용을 조작하며, 프로그래밍적으로 웹 페이지와 상호작용하는 데에 사용할 수 있습니다. 이 작업은 웹 스크래핑, 웹 애플리케이션의 자동화된 테스트, 웹사이트에서의 데이터 마이닝 등의 작업에 필수적이며, Haskell의 강력한 타입 시스템과 함수형 프로그래밍 패러다임을 활용하여 견고하고 간결한 코드를 보장합니다.

## 어떻게:

Haskell에서 HTML을 파싱하기 위해, 우리는 그것의 단순함과 유연함 때문에 `tagsoup` 라이브러리를 사용할 것입니다. 먼저, 프로젝트의 cabal 파일에 `tagsoup`를 추가하거나 `cabal install tagsoup`를 실행하여 라이브러리를 설치하십시오.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup

-- 데모를 위한 샘플 HTML
let sampleHtml = "<html><body><p>Haskell 배우기!</p><a href='http://example.com'>여기를 클릭하십시오</a></body></html>"

-- HTML을 파싱하고 링크(a 태그)를 필터링
let tags = parseTags sampleHtml
let links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

-- 추출된 링크를 출력
print links
```

샘플 출력:
```plaintext
["http://example.com"]
```

더 복잡한 HTML 파싱이 필요한 경우, 문서 변환 작업을 하고 있다면 특히 `pandoc` 라이브러리 사용을 고려하세요. 이것은 매우 다재다능하지만 복잡성이 더 높습니다:

```haskell
import Text.Pandoc

-- Pandoc 문서(doc)를 로드했다고 가정합니다, 예를 들어, 파일에서 읽기
let doc = ... -- 여기에 당신의 Pandoc 문서가 들어갑니다

-- 문서를 HTML 문자열로 변환
let htmlString = writeHtmlString def doc

-- 이제 위와 같이 `htmlString`을 파싱하거나 요구 사항에 따라 진행합니다.
```
`pandoc`은 수많은 마크업 형식 간의 변환에 중점을 둔 훨씬 더 큰 라이브러리이므로, 추가 기능이 필요하거나 이미 애플리케이션에서 문서 형식을 다루고 있다면 이를 사용하세요.
