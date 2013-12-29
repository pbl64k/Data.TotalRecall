
<!--
    Copyright 2010, 2011, 2013 Pavel Lepin
    
    This file is part of Data.TotalRecall.
    
    Data.TotalRecall is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    Data.TotalRecall is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with Data.TotalRecall.  If not, see <http://www.gnu.org/licenses/>.
-->

<xsl:stylesheet version="1.0"
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns="http://www.w3.org/1999/xhtml">

    <xsl:output method="xml" version="1.0" doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"
            doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"/>
  
    <xsl:param name="files"/>

    <xsl:template match="/">
        <html>
            <head>
                <link rel="stylesheet" type="text/css" href="css/x.css"/>
                <script type="text/javascript" src="js/x.js"/>
                <title></title>
            </head>
            <body>
                <xsl:call-template name="process-input-files">
                    <xsl:with-param name="input-files" select="$files"/>
                </xsl:call-template>
            </body>
        </html>
    </xsl:template>

    <xsl:template name="process-input-files">
        <xsl:param name="input-files"/>
        <xsl:if test="$input-files != ''">
            <xsl:variable name="head" select="substring-before($input-files, ' ')"/>
            <xsl:variable name="tail" select="substring-after($input-files, ' ')"/>
            <xsl:variable name="file-name">
                <xsl:choose>
                    <xsl:when test="$head != ''">
                        <xsl:value-of select="$head"/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select="$input-files"/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:variable>
            <div class="file">
                <div class="fileName"><xsl:value-of select="$file-name"/></div>
                <xsl:apply-templates select="document($file-name)/*"/>
            </div>
            <xsl:call-template name="process-input-files">
                <xsl:with-param name="input-files" select="$tail"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

    <xsl:template name="value">
        <div class="value">
            <xsl:apply-templates select="calculated-value"/>
            <xsl:apply-templates select="calculation-description"/>
        </div>
    </xsl:template>

    <xsl:template match="/value">
        <xsl:call-template name="value"/>
    </xsl:template>

    <xsl:template match="calculated-value">
        <div class="calculatedValue">
            <xsl:apply-templates select="node()"/>
        </div>
        <div class="calculatedValue">
            <input type="button" value="Explain" onclick="showExplanation(event);"/>
        </div>
    </xsl:template>

    <xsl:template match="calculation-description">
        <div class="calculationDescription" style="display: none;">
            <xsl:apply-templates select="structured-description"/>
        </div>
    </xsl:template>

    <xsl:template match="structured-description">
        <div>
            <xsl:apply-templates select="textual-description | inputs"/>
        </div>
    </xsl:template>

    <xsl:template match="textual-description">
        <div class="textualDescription">
            <xsl:apply-templates select="text()"/>
        </div>
    </xsl:template>

    <xsl:template match="inputs">
        <xsl:if test="*">
            <table class="inputs">
                <tbody>
                    <tr>
                        <xsl:apply-templates select="*"/>
                    </tr>
                </tbody>
            </table>
        </xsl:if>
    </xsl:template>

    <xsl:template match="inputs/value">
        <td class="inputValue">
            <xsl:call-template name="value"/>
        </td>
    </xsl:template>

    <xsl:template match="@* | node()">
        <xsl:copy>
            <xsl:apply-templates select="@* | node()"/>
        </xsl:copy>
    </xsl:template>

</xsl:stylesheet>
