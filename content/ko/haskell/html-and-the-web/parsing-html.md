---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:22.035671-07:00
description: "\uC5B4\uB5BB\uAC8C: Haskell\uC5D0\uC11C HTML\uC744 \uD30C\uC2F1\uD558\
  \uAE30 \uC704\uD574, \uC6B0\uB9AC\uB294 \uADF8\uAC83\uC758 \uB2E8\uC21C\uD568\uACFC\
  \ \uC720\uC5F0\uD568 \uB54C\uBB38\uC5D0 `tagsoup` \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB97C \uC0AC\uC6A9\uD560 \uAC83\uC785\uB2C8\uB2E4. \uBA3C\uC800, \uD504\uB85C\uC81D\
  \uD2B8\uC758 cabal \uD30C\uC77C\uC5D0 `tagsoup`\uB97C \uCD94\uAC00\uD558\uAC70\uB098\
  \ `cabal install tagsoup`\uB97C \uC2E4\uD589\uD558\uC5EC \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB97C\u2026"
lastmod: '2024-03-13T22:44:55.292563-06:00'
model: gpt-4-0125-preview
summary: "Haskell\uC5D0\uC11C HTML\uC744 \uD30C\uC2F1\uD558\uAE30 \uC704\uD574, \uC6B0\
  \uB9AC\uB294 \uADF8\uAC83\uC758 \uB2E8\uC21C\uD568\uACFC \uC720\uC5F0\uD568 \uB54C\
  \uBB38\uC5D0 `tagsoup` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD560 \uAC83\
  \uC785\uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

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
