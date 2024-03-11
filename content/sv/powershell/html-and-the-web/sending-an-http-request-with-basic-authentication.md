---
date: 2024-01-20 18:02:13.500169-07:00
description: "HTTP-f\xF6rfr\xE5gningar med grundl\xE4ggande autentisering inneb\xE4\
  r att man skickar anv\xE4ndarnamn och l\xF6senord i klartext, kodat i Base64, f\xF6\
  r att bevisa sin\u2026"
lastmod: '2024-03-11T00:14:11.509727-06:00'
model: gpt-4-1106-preview
summary: "HTTP-f\xF6rfr\xE5gningar med grundl\xE4ggande autentisering inneb\xE4r att\
  \ man skickar anv\xE4ndarnamn och l\xF6senord i klartext, kodat i Base64, f\xF6\
  r att bevisa sin\u2026"
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
---

{{< edit_this_page >}}

## Vad & Varför?
HTTP-förfrågningar med grundläggande autentisering innebär att man skickar användarnamn och lösenord i klartext, kodat i Base64, för att bevisa sin identitet. Programmerare använder det för att få åtkomst till skyddade resurser på ett enkelt och direkt sätt.

## Hur man gör:
Här är ett exempel med hjälp av Invoke-RestMethod:

```PowerShell
# Dina autentiseringsuppgifter
$anvandarNamn = 'dittAnvandarNamn'
$losenord = 'dittLosenord'

# Koda autentiseringsuppgifterna
$base64AuthInfo = [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes(("$anvandarNamn`:$losenord")))

# Skapa och skicka HTTP-förfrågan
$response = Invoke-RestMethod -Uri 'http://dittApi.se/data' -Method Get -Headers @{Authorization=("Basic $base64AuthInfo")}

# Visa svaret
$response
```

Sample output:

```PowerShell
id     : 123
name   : NamnExempel
value  : VärdeExempel
```

## Djupdykning
Grundläggande autentisering har funnits länge och ansågs vara enkel att implementera. Men det är inte det säkraste alternativet, eftersom autentiseringsuppgifterna kan lätt komprometteras om de inte skickas över en säker anslutning som HTTPS. Alternativ som OAuth och API-nycklar erbjuder mer säkerhet och används allt mer. När det gäller implementation, kom ihåg att PowerShell använder standard .NET-klasser för webbåtkomst, vilket gör det enkelt att flytta kod mellan C# och PowerShell om så krävs.

## Se även:
- [PowerShell documentation on Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Invoke-RestMethod)
- [Basic access authentication on Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
