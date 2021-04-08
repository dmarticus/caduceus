# Maintainer notes

## IBAN Data

The IBAN data is generated from the `IBAN Registry`; it's not auto-generated -- it comes from a script.

### Populating `IBAN.Data`

Run the following command to populate the `IBAN.Data` file.  N.B. This script assumes you have [poppler](https://formulae.brew.sh/formula/poppler) installed (on Mac OS) or `pdftotext` installed (on Linux).

```
> curl -O http://www.swift.com/dsp/resources/documents/IBAN_Registry.pdf
> pdftotext IBAN_REGISTRY.pdf \
  | awk -v s=0 '/^[A-Z][A-Z]/ {if (s) print} /^IBAN [sS]tructure/ {s=1} /^IBAN length/ {s=0}' | grep -v 'IBAN length' > src/Banking/IBAN/Data.hs
```

This command mostly works, but it's not guaranteed.  Once you run it you may need to edit, correct and add missing any missing data as needed.  Still, it's better than manually populating the file.  One day I'll integrate this with a 3rd-party service.
