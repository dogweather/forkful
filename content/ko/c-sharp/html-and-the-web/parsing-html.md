---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:47.309785-07:00
description: "\uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB294\uAC00: .NET\uC740 \uC6F9\
  \ \uD398\uC774\uC9C0\uB97C \uAC00\uC838\uC624\uB294 `HttpClient`\uC640 \uAC19\uC740\
  \ HTML \uC791\uC5C5\uC744 \uC704\uD55C \uAE30\uBCF8 \uC9C0\uC6D0\uC744 \uC81C\uACF5\
  \uD558\uC9C0\uB9CC, \uD1B5\uD569\uB41C \uD3EC\uAD04\uC801\uC778 HTML \uD30C\uC11C\
  \uB294 \uC81C\uACF5\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. \uB530\uB77C\uC11C \uB300\
  \uBD80\uBD84\uC758 C# \uAC1C\uBC1C\uC790\uB4E4\uC740 HtmlAgilityPack\uC774\uB098\
  \ AngleSharp\uACFC \uAC19\uC740 \uC778\uAE30\u2026"
lastmod: '2024-03-13T22:44:55.230748-06:00'
model: gpt-4-0125-preview
summary: ".NET\uC740 \uC6F9 \uD398\uC774\uC9C0\uB97C \uAC00\uC838\uC624\uB294 `HttpClient`\uC640\
  \ \uAC19\uC740 HTML \uC791\uC5C5\uC744 \uC704\uD55C \uAE30\uBCF8 \uC9C0\uC6D0\uC744\
  \ \uC81C\uACF5\uD558\uC9C0\uB9CC, \uD1B5\uD569\uB41C \uD3EC\uAD04\uC801\uC778 HTML\
  \ \uD30C\uC11C\uB294 \uC81C\uACF5\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

## 어떻게 사용하는가:
.NET은 웹 페이지를 가져오는 `HttpClient`와 같은 HTML 작업을 위한 기본 지원을 제공하지만, 통합된 포괄적인 HTML 파서는 제공하지 않습니다. 따라서 대부분의 C# 개발자들은 HtmlAgilityPack이나 AngleSharp과 같은 인기 있는 제3자 라이브러리로 전환하여 견고한 HTML 파싱 기능을 활용합니다. 두 라이브러리 모두 HTML DOM의 쉬운 쿼리, 조작 및 순회를 가능하게 합니다.

### HtmlAgilityPack 사용하기
1. **HtmlAgilityPack 설치하기**: 먼저, NuGet을 통해 프로젝트에 HtmlAgilityPack 패키지를 추가합니다.
   ```
   Install-Package HtmlAgilityPack
   ```

2. **예제 코드**: HTML 문자열을 파싱하고 모든 `<h1>` 요소의 제목을 추출합니다.

   ```csharp
   using HtmlAgilityPack;
   using System;
   using System.Linq;

   class Program
   {
       static void Main(string[] args)
       {
           var html = @"<html>
                         <body>
                             <h1>Title 1</h1>
                             <h1>Title 2</h1>
                         </body>
                        </html>";
           var htmlDoc = new HtmlDocument();
           htmlDoc.LoadHtml(html);

           var h1Tags = htmlDoc.DocumentNode.SelectNodes("//h1").Select(node => node.InnerText);
           foreach (var title in h1Tags)
           {
               Console.WriteLine(title);
           }
       }
   }
   ```

   **출력 예시:**
   ```
   Title 1
   Title 2
   ```

### AngleSharp 사용하기
1. **AngleSharp 설치하기**: NuGet을 통해 프로젝트에 AngleSharp 라이브러리를 추가합니다.
   ```
   Install-Package AngleSharp
   ```

2. **예제 코드**: HTML 문서를 로드하고 특정 클래스를 가진 `div` 요소를 쿼리합니다.

   ```csharp
   using AngleSharp;
   using AngleSharp.Dom;
   using System;
   using System.Linq;
   using System.Threading.Tasks;

   class Program
   {
       static async Task Main(string[] args)
       {
           var context = BrowsingContext.New(Configuration.Default);
           var document = await context.OpenAsync(req => req.Content("<div class='item'>Item 1</div><div class='item'>Item 2</div>"));

           var items = document.QuerySelectorAll(".item").Select(element => element.TextContent);
           foreach (var item in items)
           {
               Console.WriteLine(item);
           }
       }
   }
   ```

   **출력 예시:**
   ```
   Item 1
   Item 2
   ```

HtmlAgilityPack과 AngleSharp 모두 HTML 파싱을 위한 강력한 도구이지만, 이들 중 어느 것을 선택할지는 특정 프로젝트 요구 사항, 성능 고려 사항 또는 API 디자인에 대한 개인적인 선호도에 따라 달라질 수 있습니다.
