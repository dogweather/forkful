---
title:                "HTML 파싱"
date:                  2024-02-03T19:11:47.309785-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML 파싱"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜 사용하는가?

프로그래밍에서 HTML 파싱은 HTML 문서의 구조를 분석하고, 그 내용을 프로그래밍적으로 추출, 조작, 상호작용할 수 있게 하는 것을 말합니다. 프로그래머들은 웹 스크래핑, 데이터 추출 또는 다양한 애플리케이션을 위해 웹 페이지나 HTML 문서를 동적으로 수정하는 등의 작업을 자동화하기 위해 이를 수행합니다. 이는 웹 개발, 데이터 분석 및 자동화된 테스트 시나리오에서 필수적인 기술입니다.

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
