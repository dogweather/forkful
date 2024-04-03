---
date: 2024-01-20 17:44:06.246817-07:00
description: 'How to: (Hvordan:) .'
lastmod: '2024-03-13T22:44:40.665390-06:00'
model: gpt-4-1106-preview
summary: .
title: Nedlasting av en nettside
weight: 42
---

## How to: (Hvordan:)
```Java
import java.io.*;
import java.net.URL;

public class WebPageDownloader {
    public static void main(String[] args) {
        String webPageUrl = "http://example.com";
        String outputPath = "downloaded_page.html";

        try (InputStream in = new URL(webPageUrl).openStream();
             FileOutputStream fos = new FileOutputStream(outputPath)) {

            byte[] buffer = new byte[1024];
            int length;

            while ((length = in.read(buffer)) != -1) {
                fos.write(buffer, 0, length);
            }

            System.out.println("Webpage downloaded successfully!");
        } catch (IOException e) {
            System.err.println("Error during downloading the webpage: " + e.getMessage());
        }
    }
}
```
Utdata:
```
Webpage downloaded successfully!
```

## Deep Dive (Dypdykk)
Før internett ble allemannseie, ville å laste ned nettsider sjelden skje utenfor akademiske eller militære nettverk. Etter hvert som internett vokste, oppstod behovet for å lage sikkerhetskopier eller bearbeide informasjon eksternt. Varierte biblioteker og rammeverk tilbyr egne metoder for nedlasting – `Jsoup` og `HttpClient` for eksempel. Detaljer spiller også en rolle: å håndtere ulike tegnsett, koble til via proxy, eller håndtere omdirigeringer.

## See Also (Se også)
- [Jsoup Library](https://jsoup.org/) - for parsing og håndtering av HTML.
- [HttpClient documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html) - for mer moderne, asynkron nettverkskode.
- [Java NIO](https://docs.oracle.com/javase/8/docs/api/java/nio/package-summary.html) - for ikke-blokkerende I/O operasjoner.
